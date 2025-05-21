{assign var=is_min_max value=$other_text|strpos:"_search.gif"}
{if $is_min_max !== false}
    {assign var=form_title value="$other_text $form_title"}
{/if}

<table width="100%" cellpadding="0" cellspacing="0" border="0" class="formHeader h3Row">
    <tr>
        <td nowrap>
            <h3><span>{$form_title}</span></h3>
        </td>

        {assign var=keywords value="array('class=\"button\"', 'class=\"button\"', 'class=button', '</form>')"}
        {assign var=match value=false}

        {foreach from=$keywords  item=left}
            {if $other_text}
                {assign var=found_match value=$left|strpos:$other_text}
                {if $found_match !== false}
                    {assign var=match value=true}
                {/if}
            {/if}
        {/foreach}

        {if $other_text && $match == true}
                <td colspan="10" width="100%"><IMG height="1" width="1" src="{$blankImageURL}" alt=""></td>
            </tr>
                <tr>
                    <td width="100%" align="left" valign="middle" nowrap style="padding-bottom: 2px;">{$other_text}</td>
                    {if $show_help}
                        <td align="right" nowrap>
                            {if $REQUEST.action != "EditView"}
                                <a href="index.php?{$GLOBALS.request_string}" class="utilsLink">
                                    <img src="{$printImageURL}" alt="{$app_strings.LBL_PRINT}" border="0" align="absmiddle">
                                </a>&nbsp;
                                <a href="index.php?{$GLOBALS.request_string}" class="utilsLink">
                                    {$app_strings.LNK_PRINT}
                                </a>
                            {/if}
                            &nbsp;
                            <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module={$current_module}&help_action={$current_action}&key={$server_unique_key}"
                               class="utilsLink" target="_blank">
                                <img src="{$helpImageURL}" alt="Help" border="0" align="absmiddle">
                            </a>&nbsp;
                            <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module={$current_module}&help_action={$current_action}&key={$server_unique_key}"
                               class="utilsLink" target="_blank">
                                {$app_strings.LNK_HELP}
                            </a>
                        </td>
                {/if}
        {else}
                {if $other_text && $is_min_max == false}
                    <td width="20"><img height="1" width="20" src="{$blankImageURL}" alt=""></td>
                    <td valign="middle" nowrap width="100%">{$other_text}</td>
                {else}
                    <td width="100%"><img height="1" width="1" src="{$blankImageURL}" alt=""></td>
                {/if}
                {if $show_help}
                    <td align="right" nowrap>
                        {if $REQUEST.action != "EditView"}
                        <a href="index.php?{$GLOBALS.request_string}" class="utilsLink">
                            <img src="{$printImageURL}" alt="{$app_strings.LBL_PRINT}" border="0" align="absmiddle">
                        </a>
                        &nbsp;
                        <a href="index.php?{$GLOBALS.request_string}" class="utilsLink">
                            {$app_strings.LNK_PRINT}</a>
                        {/if}
                        &nbsp;
                        <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module={$current_module}&help_action={$current_action}&key={$server_unique_key}"
                           class="utilsLink" target="_blank">
                            <img src="{$helpImageURL}" alt="{$app_strings.LBL_HELP}" border="0" align="absmiddle">
                        </a>&nbsp;
                        <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module={$current_module}&help_action={$current_action}&key={$server_unique_key}"
                           class="utilsLink" target="_blank">{$app_strings.LNK_HELP}</a>
                    </td>
                {/if}
        {/if}
    </tr>
</table>

