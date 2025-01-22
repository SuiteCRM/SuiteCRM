<div>
    <span class="required validation-message">{$message}</span>
    <table id="questionTable" class="table table-bordered">
        <tr>
            <th>
                {$MOD.LBL_SURVEY_QUESTION}
            </th>
            <th>
                {$MOD.LBL_SURVEY_TEXT}
            </th>
            <th>
                {$MOD.LBL_SURVEY_TYPE}
            </th>
        </tr>
        {foreach from=$questions item=question}
            <tr>
                <td>
                    Q{$question.sort_order+1}
                </td>
                <td>
                    {$question.name}
                </td>
                <td>
                    {$question.type}
                </td>
            </tr>
        {/foreach}
    </table>
</div>
