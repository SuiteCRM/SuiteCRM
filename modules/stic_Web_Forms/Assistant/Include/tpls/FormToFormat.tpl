{*
********************************************************************************
 Format Form Template
********************************************************************************
*}
{* hidden Fields *}
{foreach from=$MAP.FORM.HIDDEN item=FIELD}
	<input type="hidden" id="{$FIELD.ID|default:$FIELD.NAME}" name="{$FIELD.NAME}" value="{$FIELD.VALUE}">
{/foreach}
{* // hidden fields *}

<table class='tableForm'>
    
    {* header *}
    {if $MAP.FORM.HEADER.TEXT}
        <tr class="header">
            <td colspan="4"><b><h2>{$MAP.FORM.HEADER.TEXT}</h2></b></td>
        </tr>
        <tr>
            <td colspan="4">&nbsp</td>
        </tr>
    {/if}
    {* // header *}
    
	
    {* description *}
    {if $MAP.FORM.DESCRIPTION.TEXT} 
        <tr>
            <td colspan="4">{$MAP.FORM.DESCRIPTION.TEXT}</td>
        </tr>
        <tr>
            <td colspan="4">&nbsp</td>
        </tr>
    {/if}
    {* // description *}

	{* fieldSets *}
	{foreach from=$MAP.FORM.FIELDSET item=FIELDSET}
		<tbody id="{$FIELDSET.NAME}" class="section">
			<tr>
				<td colspan="4"><h3>{$FIELDSET.LABEL}</h3></td>
			</tr>
			{foreach from=$FIELDSET.ROWS item=ROW}
				<tr>					
				{foreach from=$ROW item=COLS}
	
					{foreach from=$COLS item=FIELD}
						<td id="td_lbl_{$FIELD.name}" class="column_25" style="{if $FIELD.hidden} display: none{/if}">
							<span sugar='slot'><label id="lbl_{$FIELD.name}" for="{$FIELD.name}">{$FIELD.label}</label></span sugar='slot'>
							{if $FIELD.DEF.required}
		        				<span id="lbl_{$FIELD.name}_required" class="required">{$APP.LBL_REQUIRED_SYMBOL}</span>
		        			{/if}
		        		</td>

		        		<td id="td_{$FIELD.name}"  class="column_25" style="{if $FIELD.hidden} display: none{/if}">
	        				{* 
								TEXTAREA
								It is rendered as an input text because otherwise the editor does not work.
                                Span id = 'ta_replace' is added to be able to transform it again at the time of generating the form 
							*}
		        			<span {if $FIELD.DEF.type == 'text'} id="ta_replace" {/if} sugar="slot">
								{* Input Select *}
								{if $FIELD.DEF.type == "enum" || $FIELD.DEF.type == "radioenum"}
									<select name="{$FIELD.name}" id="{$FIELD.name}"  {$FIELD.script}>
									{html_options options=$FIELD.options selected=$FIELD.selected_options}
									</select>
								
								{* Select multiple *}
								{* // 20181214 jch case 2077 Specific square brackets are added for the name property of multiselect fields *}
								{elseif $FIELD.DEF.type == "multienum" }
									<select name="{$FIELD.name}[]" id="{$FIELD.name}" multiple="true" {$FIELD.script}>
									{html_options options=$FIELD.options selected=$FIELD.selected_options}
									</select>
								
								{* Input Checkbox *}
								{elseif $FIELD.DEF.type == "bool"}
									<input type="checkbox" id="{$FIELD.name}" name="{$FIELD.name}" {if $FIELD.DEF.default} checked {/if} {$FIELD.script}/>
								
								{* Input Date *}
								{* // 20200203 aam Removing old Calendar class and changing the input type to date and datetime-local, using HTML5 functionality *}
								{elseif $FIELD.DEF.type == "date"}
					                <input class="date_input" autocomplete="off" type="date" name="{$FIELD.name}" id="{$FIELD.name}" title="{$FIELD.name}" size="11" maxlength="10" {$FIELD.script}>
								
								{* Input Datetime *}
								{elseif $FIELD.DEF.type == "datetime" || $FIELD.DEF.type == "datetimecombo"}
									<div class="datetimecombo_time_section">
										<input class="date_input" autocomplete="off" type="date" name="{$FIELD.name}" id="{$FIELD.name}" title="{$FIELD.name}" size="11" maxlength="10" {$FIELD.script}>
										<select name="{$FIELD.name}___h" id="{$FIELD.name}___h" {$FIELD.script}>
											{html_options values=$MAP.HOURS options=$MAP.HOURS}
										</select>:
										<select name="{$FIELD.name}___m" id="{$FIELD.name}___m" {$FIELD.script}>
											{html_options values=$MAP.MINUTES options=$MAP.MINUTES}
										</select>
									</div>

								{* Input int *}    
								{elseif $FIELD.DEF.type == 'int' }
									{if $FIELD.name == 'stic_Registrations___attendees'}	
										<input id="{$FIELD.name}" name="{$FIELD.name}" type="number" value="1" {$FIELD.script}
									{else}
										<input id="{$FIELD.name}" name="{$FIELD.name}" type="number" {$FIELD.script}
									{/if}

								{* Input decimal, currency *}    
								{elseif $FIELD.DEF.type == 'decimal' || $FIELD.DEF.type == 'currency' }
									<input id="{$FIELD.name}" name="{$FIELD.name}" type="number" min="0" step="0.01" {$FIELD.script} 

								{* Input url *}    
								{elseif $FIELD.DEF.type == 'url' }
									<input id="{$FIELD.name}" name="{$FIELD.name}" type="url" {$FIELD.script} 

								{* Input varchar, url, phone, relate *}    
								{elseif $FIELD.DEF.type == 'varchar' 
									 || $FIELD.DEF.type == 'name' 
									 || $FIELD.DEF.type == 'phone' 
									 || $FIELD.DEF.type == 'relate' 								 
									 || $FIELD.DEF.type == 'email' 								 
								}
									{if ($FIELD.DEF.name == 'email1') || ($FIELD.DEF.name == 'email2')}	
										<input id="{$FIELD.name}" name="{$FIELD.name}" type="text" {$FIELD.script} 
										onchange="validateEmailAdd(this);" 				
									{else}
										<input id="{$FIELD.name}" name="{$FIELD.name}" type="text" {$FIELD.script} 
									{/if}								
									
								{* 
								TEXTAREA
								It is rendered as an input text because otherwise the editor does not work.
                               					Span id = 'ta_replace' is added to be able to transform it again at the time of generating the form
								*}
								{elseif $FIELD.DEF.type == 'text'}	
									<span id='ta_replace' sugar='slot'><input id="{$FIELD.name}" name="{$FIELD.name}" type="text" {$FIELD.script}></span>
								{/if}
							</span sugar='slot'>
						</td>
					{/foreach}
				{/foreach}
				</tr>
			{/foreach}
		</tbody>
	{/foreach}
	{* // fieldSets *}

    {* Documents *}
		{$FILES_HTML_STRING}
    {* //Documents *}


	{* buttons *}
	<tr>
	  	<td></td>
		<td>
			<input type="button" onclick="submitForm(this.form);" class="button" name="Submit" value="{$MAP.FORM.SUBMIT_LABEL|default:$MOD.LBL_WEBFORMS_SUBMIT}"/>
		</td>
	</tr>
	{* // buttons *}
	
    {* footer *}
    {if $MAP.FORM.FOOTER.TEXT}
        <tr class="footer">
            <td colspan="4">&nbsp</TD>
        </tr>	
        <tr class="footer">
            <td colspan="4"><b>{$MAP.FORM.FOOTER.TEXT}</b></td>
        </tr>
    {/if}
    {* // footer *}
</table>
