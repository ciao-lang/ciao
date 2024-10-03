/**
 * @file Common HTML/CSS/JS Ciao menu interface for Ciao.
 *
 * @author Jose F. Morales
 * @author Marco Ciccalè Baztán
 * @copyright 2017
 */

/*
 *  ciao_menu.js
 *
 *  Common HTML/CSS/JS Ciao menu interface for Ciao.
 *
 *  Copyright (C) 2017 Jose F. Morales
 *
 *  NOTE: Extracted module from ciao-playground-ui.js:
 *  - Generalize HTML tags for browser and vscode environments.
 *  - Adapted code to ES2020 JavaScript standard:
 *    - classes syntax.
 *    - prefer `const` and `let` to `var`.
 *    - for-of loops.
 *
 *  NOTE: To generate the type declarations:
 *  npx -p typescript tsc ciao-menu-html.js --declaration --allowJs --emitDeclarationOnly --outDir .
 */

/**
 * Common HTML/CSS/JS Ciao menu interface for Ciao.
 *
 * @module MenuModule
 */

/**
 * Represents an option of the Ciao menu.
 * @typedef {Object} CiaoMenuOption
 * @property {'menu_item'} kind The type of the option.
 * @property {string} menu The menu of the option.
 * @property {string} level The level of the option
 * @property {string} flag The flag represented by the option.
 * @property {string} title The title of the option.
 * @property {string} help Help about the option.
 * @property {(string[]|string)} options The options available or the type of the input option.
 * @property {string} def_opt The default option.
 * @property {Array<Array<Array<string>>>} guard Array of array of guards.
 */

// NOTE: Cannot import functions from the utils module as it imports functions
// from node. This file should not be run in a node environment, so reimplement
// the function directly

/**
 * Encapsulates a strings' double quotes and escapes them.
 * @param {string} str String to escape
 * @returns {string} Escaped string
 */
function prologQuoteString(str) {
    return `"${str.replace(/"/g, '\\"')}"`;
}

/**
 * @class Menu definition
 */
class Menu {
    /**
     * @param {string} title The title of the menu.
     * @param {CiaoMenuOption[]} menudef Definition of a menu retrieved from Ciao.
     * @param {Function} callback Callback to execute everytime a flag is updated.
     */
    constructor(title, menudef, callback) {
        /**@type {CiaoMenuOption[]} */
        this.menudef = menudef;
        /**@type {onChangeCallback} */
        this.callback = callback;
        /**@type {HTMLDivElement} */
        this.elem = document.createElement('div');
        this.elem.className = 'menu-main';
        /**@type {MenuItem[]} */
        this.menus = [];
        /**@type {Object.<string, MenuItem[]>} */
        this.menu_items_for_flag = {};

        /* Initialize the Menu */
        this.menu_insert_title(title);
        this.menu_items_load();
        this.menu_items_init();
        this.menu_items_create_elem(this.elem);
        this.menu_items_fill_elem();
    }

    /**
     * Inserts the title of the menu.
     * @param {string} title The title of the menu.
     */
    menu_insert_title(title) {
        const t = document.createElement('h1');
        t.className = 'lpdoc-cover-h1'
        t.innerText = title;
        this.elem.appendChild(t);
    }

    /**
     * Initializes the menu with the default options.
     */
    menu_items_init() {
        this.menu_items_update_def();
        this.menu_items_exec_guards();
    }

    /**
     * Loads the menu items from the menu definition.
     */
    menu_items_load() {
        this.menudef.forEach((m) => {
            this.add_menu(m);
        });
    }

    /**
     * Creates an HTML element for each option in the menu.
     * @param {HTMLElement} elm The parent HTML element.
     */
    menu_items_create_elem(elm) {
        this.menus.forEach((m) => {
            m.create_elem(elm, this.callback);
        });
    }

    /**
     * Adds a menu item to the menu.
     * @param {CiaoMenuOption} opts Options of the menu item.
     */
    add_menu(opts) {
        const { flag } = opts;
        const m = new MenuItem(this, this.callback, opts);

        if (this.menu_items_for_flag[flag] === undefined) {
            this.menu_items_for_flag[flag] = [];
        }

        this.menu_items_for_flag[flag].push(m);
        this.menus.push(m);
    }

    /**
     * Finds a menu item given the flag's name.
     * @param {string} flag Flag's identifier.
     * @returns {MenuItem|undefined} The flag's menu item or undefined if not found.
     */
    find_menu_item(flag) {
        const ms = this.menu_items_for_flag[flag];
        if (!ms) return undefined;
        for (const m of ms) {
            if (m === undefined) return undefined; /* TODO: useless sentence? */
            if (m.in_use === false) continue; /* Try next one */
            return m;
        }
        return undefined; /* None was active! */
    }

    /**
     * Retrieves the selected value of a given flag.
     * @param {string} flag Flag's identifier.
     * @returns {string|undefined} The value of the given flag or undefined if not found.
     */
    menu_value(flag) {
        const m = this.find_menu_item(flag);
        if (m === undefined) return undefined; /* TODO: useless sentence? */
        return m.get_flag_value();
    }

    /*
     * Fills and activates/deactivates all the menu items.
     */
    menu_items_fill_elem() {
        for (const m of this.menus) {
            m.fill_elem();
            m.activate(m.in_use);
        }
    }

    /**
     * Executes the guard of every element in the menu.
     */
    menu_items_exec_guards() {
        for (const m of this.menus) {
            m.exec_guard();
        }
    }

    /**
     * Sets the selected_value of every element in the menu to its default option.
     */
    menu_items_update_def() {
        for (const m of this.menus) {
            m.set_flag_def_opt();
        }
    }

    /**
     * Recomputes all the guards activating or disabling dependant menu items.
     */
    menu_items_recompute_guards() {
        for (const m of this.menus) {
            const old = m.in_use;
            m.exec_guard(this.menu_value(m.flag));
            if (old !== m.in_use) {
                m.activate(!old);
            }
        }
    }

    /**
     * Extracts all the menu values into a dictionary of triples per flag.
     * @returns {Object.<string, Array.<string>>}
     */
    extract_menu_values() {
        const menu_values = {};
        for (const key in this.menu_items_for_flag) {
            const m = this.find_menu_item(key);
            if (m !== undefined) {
                menu_values[key] = [m.menu, m.level, m.get_flag_value()];
            }
        }
        return menu_values;
    }

    /**
     * @todo Load required modules?
     * Generates a query for the top level to change the menu options.
     * @returns {string} Query to change the menu options.
     */
    get_query_for_loading_menu() {
        const menu_values_json = JSON.stringify(this.extract_menu_values());
        return `restore_menu_flags_from_json_str(${prologQuoteString(menu_values_json)})`;
    }
}

/**
 * @class {MenuItem} MenuItem class definition.
 */
class MenuItem {
    /**
     * @param {Menu} menu_ref The reference to the Menu that contains this MenuItem.
     * @param {onChangeCallback} callback Callback to execute everytime a flag is updated.
     * @param {CiaoMenuOption} opts The option to represent as a MenuItem.
     */
    constructor(
        menuref,
        callback,
        { menu, level, flag, title, help, options, def_opt, guard }
    ) {
        /**@type {Menu} */
        this.menuref = menuref;
        /**@type {onChangeCallback} */
        this.callback = callback;
        /**@type {HTMLElement} */
        this.elem = null;
        /**@type {HTMLElement} */
        this.input_elem = null;
        /**@type {HTMLElement} */
        this.help_elem = null;
        /**@type {string} */
        this.menu = menu;
        /**@type {string} */
        this.level = level;
        /**@type {string} */
        this.flag = flag;
        /**@type {string} */
        this.title = title;
        /**@type {string} */
        this.help = help;
        /**@type {(string[]|string)} */
        this.options = options;
        /**@type {string} */
        this.def_opt = def_opt;
        /**@type {Array<Array<Array<string>>>} */
        this.guard = guard; /* TODO: document (array of arrays of arrays) */
        /**@type {string} */
        this.selected_value = null;
        /**@type {boolean} */
        this.in_use = true;
        /* TODO: support 'int', 'nnegint', 'atom', 'atm', see auto_interface:hook_menu_flag_values */
        /**@type {boolean} */
        this.is_combo = Array.isArray(options);
    }

    /**
     * Creates an HTML element for the MenuItem and appends it to the parent element.
     * @param {HTMLElement} elm The parent element where the new element will be appended.
     * @param {onChangeCallback} callback Callback to execute everytime a flag is updated.
     */
    create_elem(elm, callback) {
        /*
         * <div class="menu_item menu_indented?">
         *   <div class="menu_title">
         *     Menu Level
         *     <span class="menu_flag">[menu_level]</span>
         *   </div>
         *   <select class="select">
         *     <option>naive</option>
         *     <option>expert</option>
         *   </select>
         *   <div class="menu_desc">
         *     Whether to use the naive or expert menu.
         *   </div>
         * </div>
         */
        const d = document.createElement('div');
        d.className = 'menu_item';
        // If its is a dependant menu
        if (this.title.startsWith('|')) {
            d.classList.add('menu_indented');
            // Remove the bar and trim
            this.title = this.title.slice(1).trimStart()
        }
        this.elem = d;
        /* Title and flag name */
        const dt = document.createElement('div');
        dt.className = 'menu_title';
        dt.appendChild(document.createTextNode(this.title));
        const df = document.createElement('span');
        df.className = 'menu_flag';
        df.appendChild(document.createTextNode('[' + this.flag + ']'));
        dt.appendChild(document.createTextNode('\u00A0\u00A0\u00A0'));
        dt.appendChild(df);
        d.appendChild(dt);
        /* Input for flag */
        /* Note: there may exists several elements with the same 'name'
         * property. Only the values of those activated will be considered. */
        let di;
        if (this.is_combo) {
            di = document.createElement('select');
            di.name = this.flag;
            di.className = 'select';
            const self = this;
            di.onchange = function () {
                self.update_flag_with_index(this.selectedIndex);
                self.menuref.menu_items_recompute_guards();
                callback();
            };
        } else {
            di = document.createElement('input');
            di.name = this.flag;
            di.className = 'text';
            const self = this;
            di.onchange = function () {
                self.update_flag_with_value(this.value);
                self.menuref.menu_items_recompute_guards();
                callback();
            };
        }
        this.input_elem = di;
        d.appendChild(di);
        /* Verbose description */
        const dh = document.createElement('div');
        this.help_elem = dh;
        dh.appendChild(document.createTextNode(this.help));
        dh.className = 'menu_desc';
        d.appendChild(dh);
        /* */
        elm.appendChild(d);
    }

    /**
     * Executes the guard of the MenuItem and sets its `in_use` property.
     */
    exec_guard() {
        this.in_use = this.guard.some((and) =>
            and.every((lit) => {
                /* lit = [Cond, FlagName, Value] */
                const [cond, flag_name, value] = lit;
                let fv = this.menuref.menu_value(flag_name);
                if (fv === undefined) return false;
                switch (cond) {
                    case '!=':
                        return fv !== value;
                    case '==':
                        return fv === value;
                    default:
                        throw 'unknown guard lit';
                }
            })
        );
    }

    /**
     * @todo Make this method context-specific.
     * Activates/Deactivates the HTML element.
     */
    activate(v) {
        const di = this.get_input_elem();
        const d = this.get_elem();

        di.disabled = !v;

        d.style.display = v ? 'block' : 'none';
        /* d.style.height = v ? 'auto' : 0 + 'px'; */
    }

    /**
     * Retrieves the MenuItem's input element.
     * @returns {HTMLElement}
     */
    get_input_elem() {
        return this.input_elem;
    }

    /**
     * Retrieves the MenuItem's text-field element.
     * @returns {HTMLElement}
     */
    get_text_field() {
        return this.input_elem;
    }

    /**
     * Retrieves the MenuItem's element.
     * @returns {HTMLElement}
     */
    get_elem() {
        /* TODO: wrong name */
        return this.elem;
    }

    /**
     * Set the selected value of the MenuItem to the default option.
     */
    set_flag_def_opt() {
        this.update_flag_with_value(this.def_opt); /* TODO: only if in_use? */
    }

    /**
     * Set the selected value to the provided value.
     * @param {string} value Value to be set as selected value.
     */
    update_flag_with_value(value) {
        this.selected_value = value;
    }

    /**
     * Set the selected value to the value located at the specified index.
     * @param {number} i Index of the option to select.
     */
    update_flag_with_index(i) {
        this.update_flag_with_value(this.options[i]);
    }

    /**
     * Get the selected value of the menu item.
     * @returns {string}
     */
    get_flag_value() {
        return this.selected_value;
    }

    /**
     * Fills the MenuItem's element with its options or text field.
     */
    fill_elem() {
        // if (!this.in_use) return;
        if (this.is_combo) {
            const combo = this.get_input_elem();
            clear_combo(combo);
            for (let o = 0; o < this.options.length; o++) {
                const opt = document.createElement('option');
                opt.text = this.options[o];
                opt.value = this.options[o];

                combo.options[combo.length] = opt;
                if (this.options[o] == this.get_flag_value()) {
                    combo.selectedIndex = o;
                }
            }
        } else {
            // It has to be a text box!!!
            const textf = this.get_text_field();
            textf.value = this.def_opt;
        }
    }
}

/**
 * Auxiliar function to clear a comobobox.
 * @param {HTMLSelectElement} combo The combobox to clear.
 */
function clear_combo(combo) {
    for (let c = combo.length; c > 0; c--) {
        combo.options[c] = null;
    }
}

/**
 * Auxiliar function to append a HTML element to a parent node.
 * @param {HTMLElement} elm Parent node.
 * @param {string} text Content of the new node.
 */
function add_html(elm, text) {
    elm.appendChild(elements_from_string(text));
}

/**
 * Auxiliar function to create a text HTML element.
 * @param {string} str Content of the text node.
 */
function elements_from_string(str) {
    const t = document.createElement('template');
    t.innerHTML = str;
    return t.content; /* a DocumentFragment */
}
