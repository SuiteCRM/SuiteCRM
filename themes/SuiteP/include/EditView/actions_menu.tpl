<div id="no-tab-actions">
    <ul class="nav nav-pills">
        {{if !isset($form.buttons)}}
        <li>{{sugar_button module="$module" id="EDIT" view="$view" form_id="formDetailView"}}</li>
        <li>{{sugar_button module="$module" id="DUPLICATE" view="EditView" form_id="formDetailView"}}</li>
        <li>{{sugar_button module="$module" id="DELETE" view="$view" form_id="formDetailView"}}</li>
        {{else}}
        {{counter assign="num_buttons" start=0 print=false}}
        {{foreach from=$form.buttons key=val item=button}}
        {{if !is_array($button) && in_array($button, $built_in_buttons)}}
        {{counter print=false}}
        <li>{{sugar_button module="$module" id=$button view="EditView" form_id="formDetailView"}}</li>
        {{/if}}
        {{/foreach}}
        {{if count($form.buttons) > $num_buttons}}
        {{foreach from=$form.buttons key=val item=button}}
        {{if is_array($button) && $button.customCode}}
        <li>{{sugar_button module="$module" id=$button view="EditView" form_id="formDetailView"}}</li>
        {{/if}}
        {{/foreach}}
        {{/if}}
        {{if empty($form.hideAudit) || !$form.hideAudit}}
        <li>{{sugar_button module="$module" id="Audit" view="EditView" form_id="formDetailView"}}</li>
        {{/if}}
        {{/if}}
    </ul>
</div>
