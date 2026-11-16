/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


function addLoadEvent(func) {
    var oldonload = window.onload;
    if (typeof window.onload != 'function') {
        window.onload = func;
    } else {
        window.onload = function () {
            if (oldonload) {
                oldonload();
            }
            func();
        }
    }
}

function loadDynamicEnum(field, subfield) {
    if (field != '') {
        // STIC-Custom 20260604 ART - DynamicEnum in quickcreateviews
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1217
        var el = null;
        // END STIC-Custom

        // STIC-Custom - JCH - 2022-09-26 - Add specific method for retrieving parent list value in view list.
        // STIC#865
        // Changing views and module variable
        // STIC#907
        // var el = document.getElementById(field);
        // STIC-Custom - PCS - 2023-05-19 - Enabling massupdate for dynamicenum
        // Adding recordId !== undefined condition to check if in ViewList we want to change one field or mass update
        // STIC#1109
        // STIC-Custom PCS 20240113 - Fix dynamicenum in Calendar quickcreateview
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/538
        // The variable moduleName has been moved because it is not defined at this point.
        // var moduleName = module_sugar_grp1;
        var recordId = $('input.listview-checkbox',$('form#EditView').closest('tr')).val();
        if(action_sugar_grp1 == 'index' && recordId !== undefined ){
                var moduleName = module_sugar_grp1;
        //END STIC-Custom
                var dynamicFieldName = $('form#EditView select').attr('id');
        // STIC-Custom 20260604 ART - DynamicEnum in quickcreateviews
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1217
        //         var el = $('<input></input>',{
        //         id:recordId+dynamicFieldName,
        //         type: 'hidden',
        //         value: getParentValueFromDynamicEnum(moduleName, recordId, dynamicFieldName)
        //     }).appendTo($('#'+subfield).parent());
        //     updateDynamicEnum(recordId+dynamicFieldName, subfield)
        // } else {
        //     var el = document.getElementById(field);
        // }      
        // // END STIC-Custom

        // if (el) {
        //     if (el.addEventListener) {
        //         el.addEventListener("change", function () {
        //             updateDynamicEnum(field, subfield)
        //         }, false);
        //         updateDynamicEnum(field, subfield)
        //     } else if (el.attachEvent) {
        //         el.attachEvent("onchange", function () {
        //             updateDynamicEnum(field, subfield)
        //         });
        //         updateDynamicEnum(field, subfield)
        //     }
        // }
                el = $('<input></input>',{
                id:recordId+dynamicFieldName,
                type: 'hidden',
                value: getParentValueFromDynamicEnum(moduleName, recordId, dynamicFieldName)
            }).appendTo($('#'+subfield).parent());
            updateDynamicEnum(recordId+dynamicFieldName, subfield)
        } else {
            
            // In subpanel quickcreate forms there can be duplicated IDs in the page
            // (same technical field name in parent module and subpanel module)
            // Resolve the parent field from the same form as the dynamic child first
            var subfieldElement = document.getElementById(subfield);
            if (subfieldElement) {
                var parentForm = subfieldElement.form || $(subfieldElement).closest('form')[0];
                if (parentForm && parentForm.elements && parentForm.elements[field]) {
                    el = parentForm.elements[field];
                    if (el && el.length && !el.tagName) {
                        el = el[0];
                    }
                }
            }

            // Backward compatible fallback
            if (!el) {
                el = document.getElementById(field);
            }

            // STIC-Custom 20261103 EPS - DynamicEnum child field with duplicated IDs in quickcreate subpanels
            // The child field name (subfield) can also be duplicated in the page (same technical field name
            // in the parent module and in the subpanel module, e.g. both modules having an "int_type_c"
            // field), so the global lookup may return a field of a different module (e.g. the parent
            // module detail view hidden input) and the quickcreate options are never filtered. Resolve the
            // child field from the same form as the already resolved parent field, falling back to the
            // global lookup otherwise. Similar to the parent field fix in https://github.com/SinergiaTIC/SinergiaCRM/pull/1217
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
            var scopedSubfieldElement = null;
            var resolvedParentForm = el ? (el.form || $(el).closest('form')[0]) : null;
            if (resolvedParentForm && resolvedParentForm.elements && resolvedParentForm.elements[subfield]) {
                scopedSubfieldElement = resolvedParentForm.elements[subfield];
                if (scopedSubfieldElement && scopedSubfieldElement.length && !scopedSubfieldElement.tagName) {
                    scopedSubfieldElement = scopedSubfieldElement[0];
                }
            }

            if (!scopedSubfieldElement) {
                scopedSubfieldElement = subfieldElement;
            }

            subfieldElement = scopedSubfieldElement;
            // END STIC-Custom
        }

        if (el) {
            if (el.addEventListener) {
                el.addEventListener("change", function () {
                    // STIC-Custom 20261103 EPS - pass the scoped child element (duplicated IDs in quickcreate subpanels)
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
                    // updateDynamicEnum(el, subfield)
                    updateDynamicEnum(el, subfield, subfieldElement)
                    // END STIC-Custom
                }, false);
                // STIC-Custom 20261103 EPS - pass the scoped child element (duplicated IDs in quickcreate subpanels)
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
                // updateDynamicEnum(el, subfield)
                updateDynamicEnum(el, subfield, subfieldElement)
                // END STIC-Custom
            } else if (el.attachEvent) {
                el.attachEvent("onchange", function () {
                    // STIC-Custom 20261103 EPS - pass the scoped child element (duplicated IDs in quickcreate subpanels)
                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
                    // updateDynamicEnum(el, subfield)
                    updateDynamicEnum(el, subfield, subfieldElement)
                    // END STIC-Custom
                });
                // STIC-Custom 20261103 EPS - pass the scoped child element (duplicated IDs in quickcreate subpanels)
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
                // updateDynamicEnum(el, subfield)
                updateDynamicEnum(el, subfield, subfieldElement)
                // END STIC-Custom
            }
        }
        // END STIC-Custom
    }
}


// STIC-Custom 20261103 EPS - DynamicEnum child field with duplicated IDs in quickcreate subpanels
// Added optional third parameter "subfieldElement": when loadDynamicEnum resolves the child field
// scoped to its form (duplicated IDs in the page, same technical field name in the parent module and
// the subpanel module), it is passed here to filter the correct select element.
// Extends the parent field fix of https://github.com/SinergiaTIC/SinergiaCRM/pull/1217
// https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
// function updateDynamicEnum(field, subfield) {
function updateDynamicEnum(field, subfield, subfieldElement) {
    // END STIC-Custom

    // STIC-Custom 20261103 EPS - DynamicEnum child field with duplicated IDs in quickcreate subpanels
    // The child field can also be duplicated in the page (e.g. both modules having an "int_type_c"
    // field), so the global lookup may return a field of a different module (e.g. the parent module
    // detail view hidden input) and the quickcreate options would never be filtered. Use the scoped
    // element resolved by loadDynamicEnum when provided, falling back to the global lookup otherwise.
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
    // var selector = document.getElementById(subfield);
    var selector = (typeof subfieldElement !== 'undefined' && subfieldElement != null) ? subfieldElement : document.getElementById(subfield);
    // END STIC-Custom

    if (selector != null && typeof selector.options !== 'undefined') {
        // STIC-Custom 20261103 EPS - qualify the de_entries cache key with the form scope so same-named
        // fields of different modules (e.g. stic_FollowUps and INT_Daily_LivingActivities "int_type_c",
        // which use different dropdown lists) do not share their cached options
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
        var deEntriesKey = subfield;
        if (selector.form && selector.form.id) {
            deEntriesKey = subfield + '|' + selector.form.id;
        }
        // END STIC-Custom

        // STIC-Custom 20260604 ART - DynamicEnum in quickcreateviews
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1217
        // var de_key = document.getElementById(field).value;
        // Get the value of the parent field to filter the dynamicenum values
        var parentField = (typeof field === 'string') ? document.getElementById(field) : field;

        if (!parentField) {
            return;
        }

        var de_key = parentField.value;
        // END STIC-Custom

        var current = [];
        for (var i = 0; i < selector.length; i++) {
            if (selector.options[i].selected) current.push(selector.options[i].value);
        }


        // STIC-Custom 20261103 EPS - use the form qualified cache key
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
        // if (de_entries[subfield] == null) {
        //     de_entries[subfield] = [];
        //     for (var j = 0; j < selector.options.length; j++) {
        //         de_entries[subfield][selector.options[j].value] = selector.options[j].text;
        //     }
        // }
        if (de_entries[deEntriesKey] == null) {
            de_entries[deEntriesKey] = [];
            for (var j = 0; j < selector.options.length; j++) {
                de_entries[deEntriesKey][selector.options[j].value] = selector.options[j].text;
            }
        }
        // END STIC-Custom

        // STIC-Custom 20261103 EPS - rebuild the resolved selector instead of the global lookup
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
        // document.getElementById(subfield).innerHTML = '';
        selector.innerHTML = '';
        // END STIC-Custom

        // STIC-Custom 20261103 EPS - use the form qualified cache key
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
        // for (var key in de_entries[subfield]) {
        for (var key in de_entries[deEntriesKey]) {
        // END STIC-Custom
            // STIC-Custom - PCS - 2023-05-19 - Enabling massupdate for dynamicenum
            // STIC#1109
            // if (key.indexOf(de_key + '_') == 0 || key == '') {
            if (key.indexOf(de_key + '_') == 0 || key == '' || key == '__SugarMassUpdateClearField__') {
            // END STIC-Custom
                // STIC-Custom 20261103 EPS - use the form qualified cache key
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
                // selector.options[selector.options.length] = new Option(de_entries[subfield][key], key);
                selector.options[selector.options.length] = new Option(de_entries[deEntriesKey][key], key);
                // END STIC-Custom
            }
        }

        for (var item in current) {
            for (var k = 0; k < selector.length; k++) {
                if (selector.options[k].value == current[item])
                    selector[k].selected = true;
            }
        }

        // STIC-Custom 20261103 EPS - dispatch the change event only when a valid select has been
        // resolved and rebuilt (previously it could be reached with an unresolved selector)
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
        // }
        // END STIC-Custom
        if ("createEvent" in document) {
            var evt = document.createEvent("HTMLEvents");
            evt.initEvent("change", false, true);
            selector.dispatchEvent(evt);
        }
        else
            selector.fireEvent("onchange");
    // STIC-Custom 20261103 EPS - dispatch the change event only when a valid select has been
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/1499
    }
    // END STIC-Custom
}

/**
 * 
 * @param {module} 
 * @param {*} id 
 * @param {*} field 
 * @returns 
 */
function getParentValueFromDynamicEnum(moduleName, recordId, dynamicFieldName){

    $.ajaxSetup({ async: false });
    var result = $.getJSON("index.php", {
        module: "Home",
        action: "getParentValueFromDynamicEnum",
        dynamicFieldName: dynamicFieldName,
        moduleName: moduleName,
        recordId: recordId
    });
    $.ajaxSetup({ async: true });
    return result.responseText;

}