<?php
function install_calendar_sync_hooks()
{
    installCalendarSyncHooks();
}

function installCalendarSyncHooks()
{
    $hooks = array(
        array(
            'module'         => 'Meetings',
            'hook'           => 'after_save',
            'order'          => 1,
            'description'    => 'Meeting Calendar Sync After Save',
            'file'           => 'modules/Meetings/MeetingCalendarSyncLogicHook.php',
            'class'          => 'MeetingCalendarSyncLogicHook',
            'function'       => 'afterSave',
        ),
        array(
            'module'         => 'Meetings',
            'hook'           => 'after_delete',
            'order'          => 1,
            'description'    => 'Meeting Calendar Sync After Delete',
            'file'           => 'modules/Meetings/MeetingCalendarSyncLogicHook.php',
            'class'          => 'MeetingCalendarSyncLogicHook',
            'function'       => 'afterDelete',
        ),
    );

    foreach ($hooks as $hook) {
        check_logic_hook_file($hook['module'], $hook['hook'], array($hook['order'], $hook['description'], $hook['file'], $hook['class'], $hook['function']));
    }
}
