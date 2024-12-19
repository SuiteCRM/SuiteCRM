{* 
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 *}
<h2>{$MOD.LBL_AGGREGATED_TITLE}</h2>
<h3>
    {$MOD.LBL_AGGREGATED_SUMMARY_TITLE}
    <img border="0"
        onclick="return SUGAR.util.showHelpTips(this,SUGAR.language.translate('stic_Payments','LBL_AGGREGATED_SUMMARY_TITLE_INFO'),'','' );"
        src="themes/default/images/helpInline.gif?v=qpjLeQy5v2GkTP23EP91ag" alt="Información" class="inlineHelpTip">:
</h3>
<div class="view">
    <table class="summary-items">
        <tr>
            <th></th>
        </tr>
        {foreach from=$MAP.SUMMARY key=KEY item=INFO}
            <input type="hidden" name="summary[{$KEY}]" id="summary" size=50 value="{$INFO}">
            <tr>
                {assign var="lbl_aggregated_summary_field" value="LBL_AGGREGATED_SUMMARY_"|cat:$KEY|upper}
                {if $KEY == "INCLUDED_PAYMENTS" || $KEY == "INCLUDED_ATTENDANCES"}
                    <td class="included-item">
                        {$MOD[$lbl_aggregated_summary_field]}:
                    </td>
                    <td class="included-item">
                        {$INFO}
                    </td>
                {else}
                    <td>
                        {$MOD[$lbl_aggregated_summary_field]}:
                    </td>
                    <td>
                        {$INFO}
                    </td>
                {/if}
            </tr>
        {/foreach}
        <tr>
    </table>
</div>
{if count($MAP.WARNING_LOG_ATTENDANCES) > 0}
    <h3>
        {$MOD.LBL_AGGREGATED_WARNING_LOG_ATTENDANCES_TITLE}
        <img border="0"
            onclick="return SUGAR.util.showHelpTips(this,SUGAR.language.translate('stic_Payments','LBL_AGGREGATED_WARNING_ATTENDANCES_LOG_TITLE_INFO'),'','' );"
            src="themes/default/images/helpInline.gif?v=qpjLeQy5v2GkTP23EP91ag" alt="Información" class="inlineHelpTip">:
    </h3>
    <table class="log-items view">
        <tr>
            <th width="50%">{$MOD.LBL_LIST_NAME}</th>
            <th>{$MOD.LBL_LIST_ASSIGNED_USER}</th>
            <th></th>
        </tr>
        {foreach from=$MAP.WARNING_LOG_ATTENDANCES item=ITEM}
            <tr>
                <td style="border-bottom: 0.5px inset silver;">
                    {$ITEM.name}
                </td>
                <td style="border-bottom: 0.5px inset silver;">
                    {$ITEM.assigned_user}
                </td>
                <td style="border-bottom: 0.5px inset silver;">
                    {if !empty($ITEM.assigned_user_id) }
                        <input type="button" id="{$ITEM.attendance_id}" name="{$ITEM.assigned_user_id}" onclick="sendEmail(this)"
                            value="{$MOD.LBL_AGGREGATED_NOTIFICATION_BUTTON_SEND}">
                    {/if}
                </td>
            </tr>
        {/foreach}
    </table>
{/if}
{if count($MAP.WARNING_LOG_PAYMENTS) > 0}
    <h3>
        {$MOD.LBL_AGGREGATED_WARNING_LOG_PAYMENTS_TITLE}
        <img border="0"
            onclick="return SUGAR.util.showHelpTips(this,SUGAR.language.translate('stic_Payments','LBL_AGGREGATED_WARNING_PAYMENTS_LOG_TITLE_INFO'),'','' );"
            src="themes/default/images/helpInline.gif?v=qpjLeQy5v2GkTP23EP91ag" alt="Información" class="inlineHelpTip">:
    </h3>
    <table class="log-items view">
        <tr>
            <td></td>
        </tr>
        <tr>
            <th width="50%" style="text-align: left">{$MOD.LBL_LIST_NAME}</th>
            <th style="text-align: left">{$MOD.LBL_LIST_ASSIGNED_USER}</th>
            <th style="text-align: left"></th>
        </tr>
        {foreach from=$MAP.WARNING_LOG_PAYMENTS item=ITEM}
            <tr>
                <td style="border-bottom: 0.5px inset silver;">
                    {$ITEM.name}
                </td>
                <td style="border-bottom: 0.5px inset silver;">
                    {$ITEM.assigned_user}
                </td>
            </tr>
        {/foreach}
    </table>
{/if}
<input class="return-button" type="button" onclick="window.location = 'index.php?module=stic_Payments&action=index'"
    value="{$MOD.LBL_AGGREGATED_RETURN_BUTTON}">
<input class="return-button" type="button"
    onclick="window.location = 'index.php?module=stic_Payments&action=aggregatePayments'"
    value="{$MOD.LBL_AGGREGATED_REFRESH_BUTTON}">

{literal}
    <style>
        .return-button {
            margin-top: 20px !important;
        }

        .log-items {
            padding-bottom: 10px;
        }

        .log-items th,
        td {
            padding: 10px;
            /* padding-bottom: 0px; */
        }

        .log-items td input {
            margin-bottom: 0px;
        }

        .summary-items {
            width: 35%;
        }

        .summary-items td {
            padding: 10px;
            padding-bottom: 0px;
            text-align: left;
            border-bottom: 0.5px inset silver;
            line-height: 1.7;
        }

        .included-item {
            font-weight: bold;
            /* border-bottom-color: black !important; */
        }

        .view {
            border-bottom: 15px solid white;
        }
    </style>
    <script>
        function sendEmail(element) {
            user_id = element.name;
            id = element.id;
            $('#' + id).val(SUGAR.language.get('stic_Payments', 'LBL_AGGREGATED_NOTIFICATION_BUTTON_SENDING'));
            $('#' + id).prop("disabled", true);
            $('#' + id).css("cursor", "auto");
            $('#' + id).css("opacity", 0.5);
            $.ajax({
                url: 'index.php?module=stic_Payments&action=notifyUser',
                type: 'post',
                dataType: 'json',
                data: {
                    assigned_user_id: user_id,
                },
                success: function(success) {
                    if (success) {
                        $('input[name="' + element.name + '"]').each(function() {
                            $('#' + this.id).val(SUGAR.language.get('stic_Payments',
                                'LBL_AGGREGATED_NOTIFICATION_BUTTON_SENT'));
                            $('#' + this.id).prop("disabled", true);
                            $('#' + this.id).css("cursor", "auto");
                            $('#' + this.id).css("opacity", 1);
                        });

                    } else {
                        $('input[name="' + element.name + '"]').each(() => {
                            $('#' + this.id).val(SUGAR.language.get('stic_Payments',
                                'LBL_AGGREGATED_NOTIFICATION_BUTTON_SENDING_ISSUE'));
                            $('#' + this.id).prop("disabled", true);
                            $('#' + this.id).css("cursor", "auto");
                            $('#' + this.id).css("opacity", 1);
                        });
                    }
                },
                failure: function(err) {
                    $('input[name="' + element.name + '"]').each(() => {
                        $('#' + this.id).val(SUGAR.language.get('stic_Payments',
                            'LBL_AGGREGATED_NOTIFICATION_BUTTON_SERVER_ERROR'));
                        $('#' + this.id).prop("disabled", true);
                        $('#' + this.id).css("cursor", "auto");
                        $('#' + this.id).css("opacity", 1);
                    });
                }
            });
        }
    </script>

{/literal}