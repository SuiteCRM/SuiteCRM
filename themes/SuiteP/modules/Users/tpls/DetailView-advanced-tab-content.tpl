{*
advanced tab content goes here
*}

<div id="settings_suitep" >
    <div class="row detail-view-row">
        <h4>{$MOD.LBL_USER_SETTINGS}</h4>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_RECEIVE_NOTIFICATIONS}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field">
                <!-- simple hidden start -->
                <input name='receive_notifications' class="checkbox" tabindex='12' type="checkbox" value="12" {$RECEIVE_NOTIFICATIONS} disabled>
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_MAILMERGE|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                <input tabindex='3' name='mailmerge_on' disabled class="checkbox" type="checkbox" {$MAILMERGE_ON} disabled>
                <!-- simple hidden finish -->
            </div>
        </div>
        </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_SETTINGS_URL|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field ">
                <!-- simple hidden start -->
                {$SETTINGS_URL}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_EXPORT_DELIMITER|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$EXPORT_DELIMITER}
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_EXPORT_CHARSET|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$EXPORT_CHARSET_DISPLAY}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    {if $DISPLAY_EXTERNAL_AUTH}
        <div class="row detail-view-row">
            <div class="col-xs-12 col-sm-12 detail-view-row-item">
                <!-- [hide!!] -->
                <!-- DIV inside - colspan != 3 -->
                <div class="col-xs-12 col-sm-2 label col-1-label">
                    <!-- LABEL -->
                    {$EXTERNAL_AUTH_CLASS|strip_semicolon}
                </div>
                <!-- /DIV inside  -->
                <!-- phone (version 1) -->
                <div class="col-xs-12 col-sm-10 detail-view-field " type="name" field="name">
                    <!-- simple hidden start -->
                    <input id="external_auth_only" name="external_auth_only" type="checkbox" class="checkbox" {$EXTERNAL_AUTH_ONLY_CHECKED} disabled>
                    <!-- simple hidden finish -->
                </div>
            </div>
        </div>
    {/if}
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_REMINDER|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                {include file="modules/Reminders/tpls/remindersDefaults.tpl"}
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_USE_REAL_NAMES|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                <input tabindex='3' name='use_real_names' disabled class="checkbox" type="checkbox" {$USE_REAL_NAMES} disabled>
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
</div>
<div id='locale_suitep'>
    <div class="row detail-view-row">
        <h4>{$MOD.LBL_USER_LOCALE}</h4>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_DATE_FORMAT|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                {$DATEFORMAT}
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_TIME_FORMAT|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$TIMEFORMAT}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_TIMEZONE|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                {$TIMEZONE}
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_CURRENCY|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$CURRENCY_DISPLAY}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_CURRENCY_SIG_DIGITS|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                {$CURRENCY_SIG_DIGITS}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_NUMBER_GROUPING_SEP|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                {$NUM_GRP_SEP}
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_DECIMAL_SEP|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$DEC_SEP}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_LOCALE_DEFAULT_NAME_FORMAT|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field ">
                <!-- simple hidden start -->
                {$NAME_FORMAT}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
</div>
<div id='calendar_options_suitep'>
    <div class="row detail-view-row">
        <h4>{$MOD.LBL_CALENDAR_OPTIONS}</h4>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_PUBLISH_KEY|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field ">
                <!-- simple hidden start -->
                {$CALENDAR_PUBLISH_KEY}
                <!-- simple hidden finish -->
            </div>
        </div>

    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_YOUR_PUBLISH_URL|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field wrap-text" type="name" field="name">
                <!-- simple hidden start -->
                {if $CALENDAR_PUBLISH_KEY}{$CALENDAR_PUBLISH_URL}{else}{$MOD.LBL_NO_KEY}{/if}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_SEARCH_URL|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field wrap-text">
                <!-- simple hidden start -->
                {if $CALENDAR_PUBLISH_KEY}{$CALENDAR_SEARCH_URL}{else}{$MOD.LBL_NO_KEY}{/if}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_ICAL_PUB_URL|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field wrap-text" type="name" field="name">
                <!-- simple hidden start -->
                {if $CALENDAR_PUBLISH_KEY}{$CALENDAR_ICAL_URL}{else}{$MOD.LBL_NO_KEY}{/if}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_FDOW|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                {$FDOWDISPLAY}
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
</div>
<div id='google_options_suitep style="display:{$HIDE_IF_GAUTH_UNCONFIGURED}"'>
    <div class="row detail-view-row">
        <h4>{$MOD.LBL_GOOGLE_API_SETTINGS}</h4>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_GOOGLE_API_TOKEN}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field ">
                <!-- simple hidden start -->
                Current API Token is: <span style="color:{$GOOGLE_API_TOKEN_COLOR}">{$GOOGLE_API_TOKEN}</span>
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-12 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-2 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_GSYNC_CAL}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-10 detail-view-field ">
                <!-- simple hidden start -->
                <input class="checkbox" type="checkbox" disabled {$GSYNC_CAL}>
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
</div>
<div id="layout_suitep">
    <div class="row detail-view-row">
        <h4>{$MOD.LBL_LAYOUT_OPTIONS}</h4>
    </div>
    <div class="row detail-view-row">
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_USE_GROUP_TABS}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field ">
                <!-- simple hidden start -->
                <input name="use_group_tabs" type="hidden" value="m"><input id="use_group_tabs" type="checkbox" name="use_group_tabs" {$USE_GROUP_TABS} tabindex='12' value="gm" disabled>
                <!-- simple hidden finish -->
            </div>
        </div>
        <div class="col-xs-12 col-sm-6 detail-view-row-item">
            <!-- [hide!!] -->
            <!-- DIV inside - colspan != 3 -->
            <div class="col-xs-12 col-sm-4 label col-1-label">
                <!-- LABEL -->
                {$MOD.LBL_MAILMERGE|strip_semicolon}
            </div>
            <!-- /DIV inside  -->
            <!-- phone (version 1) -->
            <div class="col-xs-12 col-sm-8 detail-view-field " type="name" field="name">
                <!-- simple hidden start -->
                <input tabindex='3' name='mailmerge_on' disabled class="checkbox" type="checkbox" {$MAILMERGE_ON} disabled>
                <!-- simple hidden finish -->
            </div>
        </div>
    </div>
</div>
