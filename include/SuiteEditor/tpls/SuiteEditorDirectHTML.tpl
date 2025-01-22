{**
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
 *}

<!-- [Direct HTML Editor implementation] -->

<script>

    /**
     * Direct HTML Editor value getter function
     *
     * @returns string - Direct HTML Editor value
     */
    SuiteEditor.getValue = function() {ldelim}
        return $('#{$elementId}').val();
    {rdelim};

    /**
     * Direct HTML Editor value setter function
     *
     * @param htmlCode
     */
    SuiteEditor.apply = function(htmlCode) {ldelim}
        if(typeof htmlCode === 'undefined') {ldelim}
            htmlCode = '';
            {rdelim}

        $('#{$elementId}').val(htmlCode);
    {rdelim};

    /**
     * Direct HTML Editor value insert function
     *
     * @param text
     * @param elemId
     */
    SuiteEditor.insert = function(text, elemId) {ldelim}
        if(typeof elemId === 'undefined') {ldelim}
            elemId = '{$elementId}';
        {rdelim}
        if(elemId != '{$elementId}') {ldelim}
            throw 'incorrect editor element id (textarea id: '+elemId+')';
        {rdelim}

        function insertAtCursor(myField, myValue) {ldelim}
            //IE support
            if (document.selection) {ldelim}
                myField.focus();
                sel = document.selection.createRange();
                sel.text = myValue;
            {rdelim}
            //MOZILLA and others
            else if (myField.selectionStart || myField.selectionStart == '0') {ldelim}
                var startPos = myField.selectionStart;
                var endPos = myField.selectionEnd;
                myField.value = myField.value.substring(0, startPos)
                        + myValue
                        + myField.value.substring(endPos, myField.value.length);
            {rdelim} else {ldelim}
                myField.value += myValue;
            {rdelim}
        {rdelim}

        insertAtCursor(document.getElementById(elemId), text);

     {rdelim};

    $(function(){ldelim}

        {if $clickHandler}
            $('#{$elementId}').click({$clickHandler});
        {/if}

        $(window).mouseup(function() {ldelim}
            $('#{$textareaId}').val(SuiteEditor.getValue());
        {rdelim});
    {rdelim});
</script>

<textarea id="{$elementId}" name="{$elementId}" cols="100" rows="25" style="width:100%"  title="">{$contents}</textarea>