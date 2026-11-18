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
        'Prospects' => 'targets',
        jjwg_Areas::class => 'jjwg-areas',
        SurveyQuestionOptions::class => 'surveyquestionoptions',
        SurveyQuestions::class => 'surveyquestions'
    ];

    /**
     * @param string $module
     *
     * @return string
     */
    public static function getIconName($module)
    {
        if (static::$iconNames[$module]) {
            return static::$iconNames[$module];
        } else {
            if (self::checkIcons($module)) {
                return strtolower(str_replace('_', '-', $module));
            } else {
                return IconRepository::DEFAULT_ICON;
            }
        }
    }

    private static function checkIcons($module) {
        $iconsPath = 'themes/SuiteP/css/suitep-base/suitepicon.json';
        $jsonStringIcons = file_get_contents($iconsPath);
        $jsonDataIcons = json_decode($jsonStringIcons, true);
        $iconPostfixes = array_keys($jsonDataIcons);

        foreach ($iconPostfixes as $item) {
            if (str_contains($item, strtolower(str_replace('_', '-', $module)))){
                return true;
            }
        }

        return false;
    }
}
