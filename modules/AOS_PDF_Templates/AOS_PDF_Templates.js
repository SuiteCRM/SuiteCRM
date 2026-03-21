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

var selected = 0;

function showVariable(fld){
    document.getElementById('variable_text').value=fld;
}

function setType(type){
    document.getElementById("type").value = type;
    populateModuleVariables(type);
}

function populateVariables(type){
    var reg_values =  regularOptions[type];

    document.getElementById('variable_name').innerHTML = '';
    document.getElementById('variable_text').value = '';

    var selector = document.getElementById('variable_name');
    for (var i in reg_values) {
        selector.options[selector.options.length] = new Option(reg_values[i], i);
    }

}

function populateModuleVariables(type){
    var mod_values =  moduleOptions[type];

    document.getElementById('module_name').innerHTML = '';

    var selector = document.getElementById('module_name');
    for (var i in mod_values) {
        selector.options[selector.options.length] = new Option(mod_values[i], i);
    }

    populateVariables(type);
}

function insert_variable(text) {
    if (text != ''){
        var inst = tinymce.get("description");
        if (inst) inst.getWin().focus();
        inst.execCommand('mceInsertContent', false, text);
        inst.execCommand('mceToggleEditor');
        inst.execCommand('mceToggleEditor');
    }
}

function insertSample(smpl){
    if(smpl != 0){
        var body = tinymce.get("description");
        var header = tinymce.get("pdfheader");
        var footer = tinymce.get("pdffooter");
        var cnf = true;
        if(body.getContent() != '' || header.getContent() != '' || footer.getContent() != ''){
            cnf=confirm(SUGAR.language.get('AOS_PDF_Templates', 'LBL_WARNING_OVERWRITE'));
        }
        if(cnf){
            smpl = JSON.parse(smpl);
            setType(smpl[0]);
            body.setContent(smpl[1]);
            header.setContent(smpl[2]);
            footer.setContent(smpl[3]);
            selected = document.getElementById('sample').options.selectedIndex;
        }
        else{
            document.getElementById('sample').options.selectedIndex =selected;
        }
    }
}
