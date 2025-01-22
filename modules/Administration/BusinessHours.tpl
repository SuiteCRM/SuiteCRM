

<form id="ConfigureSettings" name="ConfigureSettings" enctype='multipart/form-data' method="POST"
      action="index.php?module=Administration&action=BusinessHours&do=save">

<span class='error'>{$error.main}</span>

<table width="100%" cellpadding="0" cellspacing="0" border="0" class="actionsContainer">
    <tr>
        <td>
            {$BUTTONS}
        </td>
    </tr>
</table>


<table width="10%" border="0" cellspacing="1" cellpadding="0" class="edit view">




<tr><th align="left" scope="row" colspan="4"><h4>{$MOD.LBL_AOP_BUSINESS_HOURS_SETTINGS}</h4></th>
</tr>
    {foreach from=$DAY_DROPDOWNS key=day item=hours}
<tr>

    <td width="10%">{$hours.label}</td>
    <td width="5%"><label for="open_status_{$day}">{$MOD.LBL_BUSINESS_HOURS_OPEN}</label><input data-day="{$day}" type="checkbox" id="open_status_{$day}" name="open_status_{$day}" class="open_check" {if $hours.open_status}checked="checked"{/if}></td>
    <td>
     <div id="{$day}_times">{$MOD.LBL_BUSINESS_HOURS_FROM} <select name="opening_time_{$day}" tabindex="0" id="opening_time_{$day}">{$hours.opening}</select> {$MOD.LBL_BUSINESS_HOURS_TO} <select name="closing_time_{$day}" tabindex="0" id="closing_time_{$day}">{$hours.closing}</select></div>
    </td>
</tr>
    {/foreach}
</table>
<div style="padding-top: 2px;">
    {$BUTTONS}
</div>
{$JAVASCRIPT}
</form>
