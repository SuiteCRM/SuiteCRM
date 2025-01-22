<?php

#[\AllowDynamicProperties]
class IconRepository
{
    public const DEFAULT_ICON = 'default';
    public const ICON_LABELS = 'labels';
    public const ICON_FIELDS = 'fields';
    public const ICON_RELATIONSHIPS = 'relationships';
    public const ICON_LAYOUTS = 'layouts';
    public const ICON_SUBPANELS = 'labels';

    /**
     * @var array
     */
    private static $iconNames = [
        AOS_Contracts::class => 'aos-contracts-signature',
        AOR_Scheduled_Reports::class => 'aor-reports',
        'EmailTemplates' => 'emails',
        'Employees' => 'users',
        jjwg_Address_Cache::class => 'jjwg-markers',
        'ProjectTask' => 'am-tasktemplates',
        AM_ProjectTemplates::class => 'am-tasktemplates',
        'SurveyQuestionResponses' =>  'survey-responses',
        'SurveyResponses' => 'survey-responses',
        'Prospects' => 'targets'
    ];

    /**
     * @param string $module
     *
     * @return string
     */
    public static function getIconName($module)
    {
        return static::$iconNames[$module] ?? strtolower(str_replace('_', '-', $module));
    }
}
