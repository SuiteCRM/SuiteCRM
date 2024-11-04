{*
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

*}
<table border="0" cellpadding="0" cellspacing="0">
<tr valign="middle">
<td nowrap>
<input autocomplete="off" type="text" id="{{sugarvar key='name'}}_date" value="{$fields[{{sugarvar key='name' stringFormat=true}}].value}" size="11" maxlength="10" title='{{$vardef.help}}' {{if !empty($tabindex)}} tabindex='{{$tabindex}}' {{/if}}  onblur="combo_{{sugarvar key='name'}}.update(); {{if isset($displayParams.updateCallback)}}{{$displayParams.updateCallback}}{{/if}}">
{capture assign="other_attributes"}alt="{$APP.LBL_ENTER_DATE}" style="position:relative; top:6px" border="0" id="{{sugarvar key='name'}}_trigger"{/capture}
	<button id="{{sugarvar key='name'}}_trigger" class="btn btn-danger"><span class="suitepicon suitepicon-module-calendar" alt="{$APP.LBL_ENTER_DATE}"></span></button>
{{if empty($displayParams.splitDateTime)}}
</td>
<td nowrap>
{{else}}
<br>
{{/if}}
<div id="{{sugarvar key='name'}}_time_section"></div>
{{if $displayParams.showNoneCheckbox}}
<script type="text/javascript">
function set_{{sugarvar key='name'}}_values(form) {ldelim}
 if(form.{{sugarvar key='name'}}_flag.checked)  {ldelim}
	form.{{sugarvar key='name'}}_flag.value=1;
	form.{{sugarvar key='name'}}.value="";
	form.{{sugarvar key='name'}}.readOnly=true;
 {rdelim} else  {ldelim}
	form.{{sugarvar key='name'}}_flag.value=0;
	form.{{sugarvar key='name'}}.readOnly=false;
 {rdelim}
{rdelim}
</script>
{{/if}}
</td>
</tr>
{{if $displayParams.showFormats}}
<tr valign="middle">
<td nowrap>
<span class="dateFormat">{$USER_DATEFORMAT}</span>
</td>
<td nowrap>
<span class="dateFormat">{$TIME_FORMAT}</span>
</td>
</tr>
{{/if}}
</table>
<input type="hidden" id="{{sugarvar key='name'}}" name="{{sugarvar key='name'}}" value="{$fields[{{sugarvar key='name' stringFormat=true}}].value}">
<script type="text/javascript" src="{sugar_getjspath file='include/SugarFields/Fields/Datetimecombo/Datetimecombo.js'}"></script>
<script type="text/javascript">
var combo_{{sugarvar key='name'}} = new Datetimecombo("{$fields[{{sugarvar key='name' stringFormat=true}}].value}", "{{sugarvar key='name'}}", "{$TIME_FORMAT}", "{{$tabindex}}", '{{$displayParams.showNoneCheckbox}}', '{$fields[{{sugarvar key='name' stringFormat=true}}_flag].value|default:''}', true);
//Render the remaining widget fields
text = combo_{{sugarvar key='name'}}.html('{{$displayParams.updateCallback}}');
document.getElementById('{{sugarvar key='name'}}_time_section').innerHTML = text;

//Call eval on the update function to handle updates to calendar picker object
eval(combo_{{sugarvar key='name'}}.jsscript('{{$displayParams.updateCallback}}'));
</script>

<script type="text/javascript">
function update_{{sugarvar key='name'}}_available() {ldelim}
      YAHOO.util.Event.onAvailable("{{sugarvar key='name'}}_date", this.handleOnAvailable, this);
{rdelim}

update_{{sugarvar key='name'}}_available.prototype.handleOnAvailable = function(me) {ldelim}
	Calendar.setup ({ldelim}
	onClose : update_{{sugarvar key='name'}},
	inputField : "{{sugarvar key='name'}}_date",
	ifFormat : "{$CALENDAR_FORMAT}",
	daFormat : "{$CALENDAR_FORMAT}",
	button : "{{sugarvar key='name'}}_trigger",
	singleClick : true,
	step : 1,
        startWeekday: {$CALENDAR_FDOW|default:'0'},
	weekNumbers:false
	{rdelim});

	//Call update for first time to round hours and minute values
	combo_{{sugarvar key='name'}}.update(false);
{rdelim}

var obj_{{sugarvar key='name'}} = new update_{{sugarvar key='name'}}_available();
</script>
