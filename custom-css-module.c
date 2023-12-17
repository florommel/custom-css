/* Copyright (C) 2023 Florian Rommel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <emacs-module.h>
#include <gtk/gtk.h>

int plugin_is_GPL_compatible;

static emacs_value Qt;
static emacs_value Qnil;
static emacs_value symbol_name;

/*
 * The hash table `loaded_css_snippets' stores `css_snippet' objects,
 * each consisting of a `GtkCssProvider' and a priority value.
 * The GtkCssProvider objects are registered with the default GDK screen.
 *
 * We want to be able to load CSS snippets even when GTK has not yet
 * been initalized.  This is especially useful when Emacs is started
 * as a daemon where no GTK window is available at the time the init
 * file gets evaluated.
 * Before GTK initialization (`gtk_init'), we cannot create and
 * register `GtkCssProvider' objects.  Therefore, we cache the raw CSS
 * strings and delay the creation and registering of corresponding
 * `GtkCssProviders' until GTK is set up.  The elisp part of this
 * library ("custom-css.el") will probe for this condition by calling
 * `custom-css-try-init' on frame creation.  After the library has
 * been initialized (`custom-css-try-init' returns t),
 * `providers_initialized' is set to true and all cached CSS snippets
 * have been registered.  Loading new CSS snippets will now directly
 * apply them.
 */

struct css_snippet {
  void *css;  /* char* if !providers_initialized, else GtkCssProvider* */
  guint priority;
};

static GHashTable *loaded_css_snippets;

static bool providers_initialized;

#define CSS_PRIORITY_OFFSET 100

static char *
get_cstring (emacs_env *env, emacs_value str)
{
  ptrdiff_t len = 0;
  env->copy_string_contents (env, str, NULL, &len);
  char *buf = malloc (len);
  if (env->copy_string_contents (env, str, buf, &len))
    return buf;
  else
    free (buf);
  return NULL;
}

static char *
get_symbol_name (emacs_env *env, emacs_value symbol)
{
  emacs_value args[] = {symbol};
  emacs_value name = env->funcall (env, symbol_name, 1, args);
  return get_cstring (env, name);
}

emacs_value
Fcustom_css_load (emacs_env *env, ptrdiff_t nargs,
                  emacs_value args[], void *data)
{
  char *css_string = get_cstring (env, args[1]);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    goto exit;

  char *symbol = get_symbol_name (env, args[0]);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    goto cleanup;

  intmax_t prio = (nargs == 2) ? 0 : env->extract_integer (env, args[2]);
  intmax_t effective_priority = GTK_STYLE_PROVIDER_PRIORITY_USER
                                + CSS_PRIORITY_OFFSET + prio;

  struct css_snippet *snippet = malloc (sizeof (struct css_snippet));
  snippet->priority = effective_priority;

  if (providers_initialized)
    {
      GdkScreen *screen = gdk_screen_get_default ();

      struct css_snippet *old = g_hash_table_lookup (loaded_css_snippets, symbol);
      if (old)
        gtk_style_context_remove_provider_for_screen (screen,
                                                      GTK_STYLE_PROVIDER (old->css));

      GtkCssProvider *prov = gtk_css_provider_new ();
      gtk_css_provider_load_from_data (prov, css_string, -1, NULL);
      gtk_style_context_add_provider_for_screen (screen,
                                                 GTK_STYLE_PROVIDER (prov),
                                                 effective_priority);
      snippet->css = prov;
      free (css_string);
    }
  else
    {
      snippet->css = css_string;
    }

  g_hash_table_insert (loaded_css_snippets, symbol, snippet);
exit:
  return Qt;
cleanup:
  free (css_string);
  return Qt;
}

emacs_value
Fcustom_css_unload (emacs_env *env, ptrdiff_t nargs,
                    emacs_value args[], void *data)
{
  char *symbol = get_symbol_name (env, args[0]);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    goto exit;

  if (providers_initialized)
    {
      GdkScreen *screen = gdk_screen_get_default ();
      struct css_snippet *snippet = g_hash_table_lookup (loaded_css_snippets, symbol);
      if (snippet)
        gtk_style_context_remove_provider_for_screen (screen,
                                                      GTK_STYLE_PROVIDER (snippet->css));
    }

  g_hash_table_remove (loaded_css_snippets, symbol);
  free (symbol);
exit:
  return Qt;
}

emacs_value
Fcustom_css_unload_all (emacs_env *env, ptrdiff_t nargs,
                        emacs_value args[], void *data)
{
  if (providers_initialized)
    {
      GdkScreen *screen = gdk_screen_get_default ();
      GHashTableIter iter;
      gpointer key, snippet;
      g_hash_table_iter_init (&iter, loaded_css_snippets);
      while (g_hash_table_iter_next (&iter, &key, &snippet))
        {
          void *prov = ((struct css_snippet *)snippet)->css;
          gtk_style_context_remove_provider_for_screen (screen,
                                                        GTK_STYLE_PROVIDER (prov));
        }
    }
  g_hash_table_remove_all (loaded_css_snippets);
  return Qt;
}

emacs_value
Fcustom_css_try_init (emacs_env *env, ptrdiff_t nargs,
                      emacs_value args[], void *data)
{
  GdkScreen *screen = gdk_screen_get_default ();
  if (screen && !providers_initialized)
    {
      GHashTableIter iter;
      gpointer key, snippet;
      g_hash_table_iter_init (&iter, loaded_css_snippets);
      while (g_hash_table_iter_next (&iter, &key, &snippet))
        {
          GtkCssProvider *prov = gtk_css_provider_new ();
          char *css_string = ((struct css_snippet *)snippet)->css;
          guint effective_priority = ((struct css_snippet *)snippet)->priority;
          gtk_css_provider_load_from_data (prov, css_string, -1, NULL);
          gtk_style_context_add_provider_for_screen (screen,
                                                     GTK_STYLE_PROVIDER (prov),
                                                     effective_priority);
          ((struct css_snippet *)snippet)->css = prov;
          free (css_string);
        }
      providers_initialized = true;
    }
  return screen ? Qt : Qnil;
}

static void
free_css_snippet (void *snippet)
{
  if (providers_initialized)
    g_object_unref (((struct css_snippet*)snippet)->css);
  else
    free (((struct css_snippet*)snippet)->css);
  free (snippet);
}

static void
def_func (emacs_env *env, emacs_function fn, int min, int max,
          char *name, char *desc)
{
  emacs_value func = env->make_function (env, min, max, fn, desc, NULL);
  emacs_value symbol = env->intern (env, name);
  emacs_value args[] = {symbol, func};
  env->funcall (env, env->intern (env, "defalias"), 2, args);
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  if (runtime->size < sizeof (*runtime))
    return 1;

  emacs_env *env = runtime->get_environment(runtime);

  Qt = env->make_global_ref (env, env->intern(env, "t"));
  Qnil = env->make_global_ref (env, env->intern(env, "nil"));
  symbol_name = env->make_global_ref (env, env->intern (env, "symbol-name"));

  loaded_css_snippets = g_hash_table_new_full(g_str_hash, g_str_equal,
                                              free, free_css_snippet);

  def_func (env, Fcustom_css_load, 2, 3, "custom-css-load",
            "Load and apply a CSS snippet into Emacs.\n"
            "The first argument is a symbol that is used as a handle for the CSS snippet.\n"
            "The second argument is the CSS string to be loaded.\n"
            "If a CSS snipped with the given identifier already exists it is replaced.\n"
            "The third optional argument specifies the priority as an integer (default: 0).");
  def_func (env, Fcustom_css_unload, 1, 1, "custom-css-unload",
            "Unload and remove a previously loaded CSS snippet.\n"
            "Removes the CSS snippet identified by the given symbol.");
  def_func (env, Fcustom_css_unload_all, 0, 0, "custom-css-unload-all",
            "Unload all loaded custom CSS snippets.");
  def_func (env, Fcustom_css_try_init, 0, 0, "custom-css-try-init",
            "Try to initialize custom-css.\n"
            "Returns t if the initialization was successful, otherwise nil.\n"
            "You should not need to call this manually.\n"
            "Successful initialization is necessary to make the loaded CSS snippets visible.\n"
            "Note, that you can load CSS snippets before initialization.\n"
            "The snippets become visible as soon as the initialization was successful.");

  return 0;
}
