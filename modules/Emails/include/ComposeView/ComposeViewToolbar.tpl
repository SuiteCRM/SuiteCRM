<div class="panel panel-default panel-email-compose">
    <div class="panel-body">
        {*<input title="Save" accesskey="a" class="button primary" onclick="" type="submit" name="button" value="Save" id="SAVE">*}
        {*<input title="Cancel [Alt+l]" accesskey="l" class="button" onclick="SUGAR.ajaxUI.loadContent('index.php?action=DetailView&amp;module=FP_events&amp;record=cb676657-b74f-c928-9967-58c146121ee9'); return false;" name="button" value="Cancel" type="button" id="CANCEL">*}
         {{if !empty($form) && !empty($form.buttons)}}
            <div class="custom-buttons">
                 {{foreach from=$form.buttons key=val item=button}}
                    {{sugar_button module="$module" id=$button form_id="$form_id" view="$view"}}
                 {{/foreach}}
            </div>
         {{/if}}
    </div>
</div>
