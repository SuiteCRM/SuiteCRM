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
{{include file=$headerTpl}}
{sugar_include include=$includes}
<div id="{{$module}}_detailview_tabs"
{{if $useTabs}}
class="yui-navset detailview_tabs"
{{/if}}
>
    {{if $useTabs}}
    {* Generate the Tab headers *}
    {{counter name="tabCount" start=-1 print=false assign="tabCount"}}
    <ul class="yui-nav">
    {{foreach name=section from=$sectionPanels key=label item=panel}}
        {{capture name=label_upper assign=label_upper}}{{$label|upper}}{{/capture}}
        {* override from tab definitions *}
        {{if (isset($tabDefs[$label_upper].newTab) && $tabDefs[$label_upper].newTab == true)}}
            {{counter name="tabCount" print=false}}
            <li><a id="tab{{$tabCount}}" href="javascript:void(0)"><em>{sugar_translate label='{{$label}}' module='{{$module}}'}</em></a></li>
        {{/if}}
    {{/foreach}}
        {{counter name="tabCount" print=false}}
        <li {if $IS_GROUP_OR_PORTAL}style="display: none;"{/if}>
            <a id="tab{{$tabCount}}" href="javascript:void(0)"><em>{$MOD.LBL_ADVANCED}</em></a>
        </li>
        {{counter name="tabCount" print=false}}
        {if $SHOW_ROLES}
            <li>
                <a id="tab{{$tabCount}}" href="javascript:void(0)"><em>{$MOD.LBL_USER_ACCESS}</em></a>
            </li>
        {/if}
    </ul>
    {{/if}}
    <div {{if $useTabs}}class="yui-content"{{/if}}>
{{* Loop through all top level panels first *}}
{{counter name="panelCount" print=false start=0 assign="panelCount"}}
{{counter name="tabCount" start=-1 print=false assign="tabCount"}}
{{foreach name=section from=$sectionPanels key=label item=panel}}
{{assign var='panel_id' value=$panelCount}}
{{capture name=label_upper assign=label_upper}}{{$label|upper}}{{/capture}}
  {{if (isset($tabDefs[$label_upper].newTab) && $tabDefs[$label_upper].newTab == true)}}
    {{counter name="tabCount" print=false}}
    {{if $tabCount != 0}}</div>{{/if}}
    <div id='tabcontent{{$tabCount}}'>
  {{/if}}

    {{if ( isset($tabDefs[$label_upper].panelDefault) && $tabDefs[$label_upper].panelDefault == "collapsed" && isset($tabDefs[$label_upper].newTab) && $tabDefs[$label_upper].newTab == false) }}
        {{assign var='panelState' value=$tabDefs[$label_upper].panelDefault}}
    {{else}}
        {{assign var='panelState' value="expanded"}}
    {{/if}}
<div id='detailpanel_{{$smarty.foreach.section.iteration}}' class='detail view  detail508 {{$panelState}}'>
{counter name="panelFieldCount" start=0 print=false assign="panelFieldCount"}
{{* Print out the panel title if one exists*}}

{{* Check to see if the panel variable is an array, if not, we'll attempt an include with type param php *}}
{{* See function.sugar_include.php *}}
{{if !is_array($panel)}}
    {sugar_include type='php' file='{{$panel}}'}
{{else}}

    {{if !empty($label) && !is_int($label) && $label != 'DEFAULT' && (!isset($tabDefs[$label_upper].newTab) || (isset($tabDefs[$label_upper].newTab) && $tabDefs[$label_upper].newTab == false))}}
    <h4>
      <a href="javascript:void(0)" class="collapseLink" onclick="collapsePanel({{$smarty.foreach.section.iteration}});">
      <img border="0" id="detailpanel_{{$smarty.foreach.section.iteration}}_img_hide" src="{sugar_getimagepath file="basic_search.gif"}"></a>
      <a href="javascript:void(0)" class="expandLink" onclick="expandPanel({{$smarty.foreach.section.iteration}});">
      <img border="0" id="detailpanel_{{$smarty.foreach.section.iteration}}_img_show" src="{sugar_getimagepath file="advanced_search.gif"}"></a>
      {sugar_translate label='{{$label}}' module='{{$module}}'}
    {{if isset($panelState) && $panelState == 'collapsed'}}
    <script>
      document.getElementById('detailpanel_{{$smarty.foreach.section.iteration}}').className += ' collapsed';
    </script>
    {{else}}
    <script>
      document.getElementById('detailpanel_{{$smarty.foreach.section.iteration}}').className += ' expanded';
    </script>
    {{/if}}
    </h4>

    {{/if}}
	{{* Print out the table data *}}
	<!-- PANEL CONTAINER HERE.. -->
  <table id='{{$label}}' class="panelContainer" cellspacing='{$gridline}'>



	{{foreach name=rowIteration from=$panel key=row item=rowData}}
	{counter name="fieldsUsed" start=0 print=false assign="fieldsUsed"}
	{counter name="fieldsHidden" start=0 print=false assign="fieldsHidden"}
	{capture name="tr" assign="tableRow"}
	<tr>
		{{assign var='columnsInRow' value=$rowData|@count}}
		{{assign var='columnsUsed' value=0}}
		{{foreach name=colIteration from=$rowData key=col item=colData}}
	    {{if !empty($colData.field.hideIf)}}
	    	{if !({{$colData.field.hideIf}}) }
	    {{/if}}
			{counter name="fieldsUsed"}
			{{if empty($colData.field.hideLabel)}}
			<td width='{{$def.templateMeta.widths[$smarty.foreach.colIteration.index].label}}%' scope="col">
				{{if !empty($colData.field.name)}}
				    {if !$fields.{{$colData.field.name}}.hidden}
                {{/if}}
				{{if isset($colData.field.customLabel)}}
			       {{$colData.field.customLabel}}
				{{elseif isset($colData.field.label) && strpos($colData.field.label, '$')}}
				   {capture name="label" assign="label"}{{$colData.field.label}}{/capture}
			       {$label|strip_semicolon}:
				{{elseif isset($colData.field.label)}}
				   {capture name="label" assign="label"}{sugar_translate label='{{$colData.field.label}}' module='{{$module}}'}{/capture}
			       {$label|strip_semicolon}:
				{{elseif isset($fields[$colData.field.name])}}
				   {capture name="label" assign="label"}{sugar_translate label='{{$fields[$colData.field.name].vname}}' module='{{$module}}'}{/capture}
			       {$label|strip_semicolon}:
				{{else}}
				   &nbsp;
				{{/if}}
                {{if isset($colData.field.popupHelp) || isset($fields[$colData.field.name]) && isset($fields[$colData.field.name].popupHelp) }}
                   {{if isset($colData.field.popupHelp) }}
                     {capture name="popupText" assign="popupText"}{sugar_translate label="{{$colData.field.popupHelp}}" module='{{$module}}'}{/capture}
                   {{elseif isset($fields[$colData.field.name].popupHelp)}}
                     {capture name="popupText" assign="popupText"}{sugar_translate label="{{$fields[$colData.field.name].popupHelp}}" module='{{$module}}'}{/capture}
                   {{/if}}
                   {sugar_help text=$popupText WIDTH=400}
                {{/if}}
                {{if !empty($colData.field.name)}}
                {/if}
                {{/if}}
                {{/if}}
			</td>
			<td class="{{if $inline_edit && !empty($colData.field.name) && ($fields[$colData.field.name].inline_edit == 1 || !isset($fields[$colData.field.name].inline_edit))}}inlineEdit{{/if}}" type="{{$fields[$colData.field.name].type}}" field="{{$fields[$colData.field.name].name}}" width='{{$def.templateMeta.widths[$smarty.foreach.colIteration.index].field}}%' {{if $colData.colspan}}colspan='{{$colData.colspan}}'{{/if}} {{if isset($fields[$colData.field.name].type) && $fields[$colData.field.name].type == 'phone'}}class="phone"{{/if}}>
			    {{if !empty($colData.field.name)}}
			    {if !$fields.{{$colData.field.name}}.hidden}
			    {{/if}}
				{{$colData.field.prefix}}
				{{if ($colData.field.customCode && !$colData.field.customCodeRenderField) || $colData.field.assign}}
					{counter name="panelFieldCount"}
					<span id="{{$colData.field.name}}" class="sugar_field">{{sugar_evalcolumn var=$colData.field colData=$colData}}</span>
				{{elseif $fields[$colData.field.name] && !empty($colData.field.fields) }}
				    {{foreach from=$colData.field.fields item=subField}}
				        {{if $fields[$subField]}}
				        	{counter name="panelFieldCount"}
				            {{sugar_field parentFieldArray='fields' tabindex=$tabIndex vardef=$fields[$subField] displayType='DetailView'}}&nbsp;

				        {{else}}
				        	{counter name="panelFieldCount"}
				            {{$subField}}
				        {{/if}}
				    {{/foreach}}
				{{elseif $fields[$colData.field.name]}}
					{counter name="panelFieldCount"}
					{{sugar_field parentFieldArray='fields' vardef=$fields[$colData.field.name] displayType='DetailView' displayParams=$colData.field.displayParams typeOverride=$colData.field.type}}

				{{/if}}
				{{if !empty($colData.field.customCode) && $colData.field.customCodeRenderField}}
				    {counter name="panelFieldCount"}
				    <span id="{{$colData.field.name}}" class="sugar_field">{{sugar_evalcolumn var=$colData.field colData=$colData}}</span>
                {{/if}}
				{{$colData.field.suffix}}
				{{if !empty($colData.field.name)}}
				{/if}
				{{/if}}

				{{if $inline_edit && !empty($colData.field.name) && ($fields[$colData.field.name].inline_edit == 1 || !isset($fields[$colData.field.name].inline_edit))}}<div class="inlineEditIcon"> {sugar_getimage name="inline_edit_icon.svg" attr='border="0" ' alt="$alt_edit"}</div>{{/if}}
			</td>
	    {{if !empty($colData.field.hideIf)}}
			{else}

			<td>&nbsp;</td><td>&nbsp;</td>
			{/if}
	    {{/if}}

		{{/foreach}}
	</tr>
	{/capture}
	{if $fieldsUsed > 0 && $fieldsUsed != $fieldsHidden}
	{$tableRow}
	{/if}
	{{/foreach}}
	</table>
    {{if !empty($label) && !is_int($label) && $label != 'DEFAULT' && (!isset($tabDefs[$label_upper].newTab) || (isset($tabDefs[$label_upper].newTab) && $tabDefs[$label_upper].newTab == false))}}
    <script type="text/javascript">SUGAR.util.doWhen("typeof initPanel == 'function'", function() {ldelim} initPanel({{$smarty.foreach.section.iteration}}, '{{$panelState}}'); {rdelim}); </script>
    {{/if}}
{{/if}}
</div>

{if $panelFieldCount == 0}

<script>document.getElementById("{{$label}}").style.display='none';</script>
{/if}
{{/foreach}}
{{if $useTabs}}
  </div>
{{/if}}
 {{counter name="tabCount" print=false}}
    <div id='tabcontent{{$tabCount}}'>
        <div id="detailpanel_{{$tabCount+1}}" class="detail view  detail508 expanded">
            <div id="settings">
                <table width="100%" border="0" cellspacing="0" cellpadding="0" class="detail view">
                    <tr>
                        <th colspan='4' align="left" width="100%" valign="top">
                            <h4>
                                <span>{$MOD.LBL_USER_SETTINGS}</span>
                            </h4>
                        </th>
                    </tr>
                    <tr>
                        <td scope="row">
                            <span>{$MOD.LBL_RECEIVE_NOTIFICATIONS|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span><input class="checkbox" type="checkbox" disabled {$RECEIVE_NOTIFICATIONS}></span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_RECEIVE_NOTIFICATIONS_TEXT}&nbsp;</span>
                        </td>
                    </tr>

                    <tr>
                        <td scope="row" valign="top">
                            <span>{$MOD.LBL_REMINDER|strip_semicolon}:
                        </td>
                        <!--
                    <td valign="top" nowrap><span>{include file="modules/Meetings/tpls/reminders.tpl"}</span></td>
                    -->
                        <td valign="top" nowrap>
                            <span>{include file="modules/Reminders/tpls/remindersDefaults.tpl"}</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_REMINDER_TEXT}&nbsp;</span>
                        </td>

                    </tr>

                    <tr>
                        <td valign="top" scope="row">
                            <span>{$MOD.LBL_MAILMERGE|strip_semicolon}:</span>
                        </td>
                        <td valign="top" nowrap>
                            <span><input tabindex='3' name='mailmerge_on' disabled class="checkbox"
                                         type="checkbox" {$MAILMERGE_ON}></span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_MAILMERGE_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td valign="top" scope="row">
                            <span>{$MOD.LBL_SETTINGS_URL|strip_semicolon}:</span>
                        </td>
                        <td valign="top" nowrap>
                            <span>{$SETTINGS_URL}</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_SETTINGS_URL_DESC}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td scope="row" valign="top">
                            <span>{$MOD.LBL_EXPORT_DELIMITER|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$EXPORT_DELIMITER}</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_EXPORT_DELIMITER_DESC}</span>
                        </td>
                    </tr>
                    <tr>
                        <td scope="row" valign="top">
                            <span>{$MOD.LBL_EXPORT_CHARSET|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$EXPORT_CHARSET_DISPLAY}</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_EXPORT_CHARSET_DESC}</span>
                        </td>
                    </tr>
                    <tr>
                        <td scope="row" valign="top">
                            <span>{$MOD.LBL_USE_REAL_NAMES|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span><input tabindex='3' name='use_real_names' disabled class="checkbox"
                                         type="checkbox" {$USE_REAL_NAMES}></span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_USE_REAL_NAMES_DESC}</span>
                        </td>
                    </tr>
                    {if $DISPLAY_EXTERNAL_AUTH}
                        <tr>
                            <td scope="row" valign="top">
                                <span>{$EXTERNAL_AUTH_CLASS|strip_semicolon}:</span>
                            </td>
                            <td valign="top" nowrap>
                                <span><input id="external_auth_only" name="external_auth_only" type="checkbox"
                                             class="checkbox" {$EXTERNAL_AUTH_ONLY_CHECKED}></span>
                            </td>
                            <td>
                                <span>{$MOD.LBL_EXTERNAL_AUTH_ONLY} {$EXTERNAL_AUTH_CLASS}</span>
                            </td>
                        </tr>
                    {/if}
                </table>
            </div>
            <div id='locale'>
                <table width="100%" border="0" cellspacing="0" cellpadding="0" class="detail view">
                    <tr>
                        <th colspan='4' align="left" width="100%" valign="top">
                            <h4>
                                <span>{$MOD.LBL_USER_LOCALE}</span>
                            </h4>
                        </th>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_DATE_FORMAT|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$DATEFORMAT}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_DATE_FORMAT_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_TIME_FORMAT|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$TIMEFORMAT}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_TIME_FORMAT_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_TIMEZONE|strip_semicolon}:</span>
                        </td>
                        <td nowrap>
                            <span>{$TIMEZONE}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_ZONE_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_CURRENCY|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$CURRENCY_DISPLAY}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_CURRENCY_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_CURRENCY_SIG_DIGITS|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$CURRENCY_SIG_DIGITS}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_CURRENCY_SIG_DIGITS_DESC}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_NUMBER_GROUPING_SEP|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$NUM_GRP_SEP}&nbsp;</span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_NUMBER_GROUPING_SEP_TEXT}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_DECIMAL_SEP|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$DEC_SEP}&nbsp;</span>
                        </td>
                        <td>
                            <span></span>{$MOD.LBL_DECIMAL_SEP_TEXT}&nbsp;</td>
                    </tr>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_LOCALE_DEFAULT_NAME_FORMAT|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$NAME_FORMAT}&nbsp;</span>
                        </td>
                        <td>
                            <span></span>{$MOD.LBL_LOCALE_NAME_FORMAT_DESC}&nbsp;</td>
                    </tr>
                </table>
            </div>
            <div id='calendar_options'>
                <table width="100%" border="0" cellspacing="0" cellpadding="0" class="detail view">
                    <tr>
                        <th colspan='4' align="left" width="100%" valign="top">
                            <h4>
                                <span>{$MOD.LBL_CALENDAR_OPTIONS}</span>
                            </h4>
                        </th>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_PUBLISH_KEY|strip_semicolon}:</span>
                        </td>
                        <td width="20%">
                            <span>{$CALENDAR_PUBLISH_KEY}&nbsp;</span>
                        </td>
                        <td width="65%">
                            <span>{$MOD.LBL_CHOOSE_A_KEY}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>
                                <nobr>{$MOD.LBL_YOUR_PUBLISH_URL|strip_semicolon}:</nobr>
                            </span>
                        </td>
                        <td colspan=2>{if $CALENDAR_PUBLISH_KEY}{$CALENDAR_PUBLISH_URL}{else}{$MOD.LBL_NO_KEY}{/if}</td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_SEARCH_URL|strip_semicolon}:</span>
                        </td>
                        <td colspan=2>
                            <span>{if $CALENDAR_PUBLISH_KEY}{$CALENDAR_SEARCH_URL}{else}{$MOD.LBL_NO_KEY}{/if}</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_ICAL_PUB_URL|strip_semicolon}: {sugar_help text=$MOD.LBL_ICAL_PUB_URL_HELP}</span>
                        </td>
                        <td colspan=2>
                            <span>{if $CALENDAR_PUBLISH_KEY}{$CALENDAR_ICAL_URL}{else}{$MOD.LBL_NO_KEY}{/if}</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_FDOW|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span>{$FDOWDISPLAY}&nbsp;</span>
                        </td>
                        <td>
                            <span></span>{$MOD.LBL_FDOW_TEXT}&nbsp;</td>
                    </tr>
                </table>
            </div>
            <div id='edit_tabs'>
                <table width="100%" border="0" cellspacing="0" cellpadding="0" class="detail view">
                    <tr>
                        <th colspan='4' align="left" width="100%" valign="top">
                            <h4>
                                <span>{$MOD.LBL_LAYOUT_OPTIONS}</span>
                            </h4>
                        </th>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_USE_GROUP_TABS|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span><input class="checkbox" type="checkbox" disabled {$USE_GROUP_TABS}></span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_NAVIGATION_PARADIGM_DESCRIPTION}&nbsp;</span>
                        </td>
                    </tr>
                    <tr>
                        <td width="15%" scope="row">
                            <span>{$MOD.LBL_SUBPANEL_TABS|strip_semicolon}:</span>
                        </td>
                        <td>
                            <span><input class="checkbox" type="checkbox" disabled {$SUBPANEL_TABS}></span>
                        </td>
                        <td>
                            <span>{$MOD.LBL_SUBPANEL_TABS_DESCRIPTION}&nbsp;</span>
                        </td>
                    </tr>
                </table>
            </div>
        </div>
</div>
   {{counter name="tabCount" print=false}}
    <div id='tabcontent{{$tabCount}}'>
        <div id="detailpanel_{{$tabCount+1}}" class="detail view  detail508 expanded">
            <div id="advanced">
                 {{$ROLE_HTML}}
             </div>
        </div>

</div>
</div>
{{include file=$footerTpl}}
{{if $useTabs}}
<script type='text/javascript' src='{sugar_getjspath file='modules/javascript/popup_helper.js'}'></script>
<script type="text/javascript" src="{sugar_getjspath file='cache/include/javascript/sugar_grp_yui_widgets.js'}"></script>
<script type="text/javascript">
var {{$module}}_detailview_tabs = new YAHOO.widget.TabView("{{$module}}_detailview_tabs");
{{$module}}_detailview_tabs.selectTab(0);
</script>
{{/if}}
<script type="text/javascript" src="include/InlineEditing/inlineEditing.js"></script>
<script type="text/javascript" src="modules/Favorites/favorites.js"></script>
<script type='text/javascript' src='{sugar_getjspath file='modules/Users/DetailView.js'}'></script>