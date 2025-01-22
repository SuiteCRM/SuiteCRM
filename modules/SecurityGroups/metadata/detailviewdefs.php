<?php

$module_name = 'SecurityGroups';
$viewdefs[$module_name]['DetailView'] =
    array(
        'templateMeta' =>
            array(
                'form' =>
                    array(
                        'buttons' =>
                            array(
                                'EDIT', 'DUPLICATE', 'DELETE',
                            )),
                'maxColumns' => '2',
                'widths' => array(
                    0 =>
                        array(
                            'label' => '10',
                            'field' => '30'
                        ),
                    1 =>
                        array(
                            'label' => '10',
                            'field' => '30'
                        )
                ),
            ),

        'panels' =>
            array(
                'LBL_PANEL_OVERVIEW' =>
                    array(
                        0 =>
                            array(
                                'name',
                                'assigned_user_name',
                            ),


                        array(
                            array(
                                'name' => 'date_entered',
                                'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
                                'label' => 'LBL_DATE_ENTERED',
                            ),
                            array(
                                'name' => 'date_modified',
                                'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
                                'label' => 'LBL_DATE_MODIFIED',
                            ),
                        ),
                        array(
                            'noninheritable',
                        ),
                        array(
                            'description',
                        ),
                    ),
            )
    );
