<?php
/**
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
 */

 /**
 * This script contains Rector config in order to run Rector during the update of SinergiaCRM instances. 
 * This Rector configuration applies the rules to migrate to PHP 8 all files in custom folder and non standard modules 
 */
declare(strict_types=1);

use Rector\Config\RectorConfig;

use Rector\Set\ValueObject\LevelSetList;

use Rector\Php52\Rector\Property\VarToPublicPropertyRector;
use Rector\Php52\Rector\Switch_\ContinueToBreakInSwitchRector;
use Rector\Removing\Rector\FuncCall\RemoveFuncCallArgRector;
use Rector\Removing\ValueObject\RemoveFuncCallArg;

use Rector\Php53\Rector\FuncCall\DirNameFileConstantToDirConstantRector;
use Rector\Php53\Rector\Ternary\TernaryToElvisRector;
use Rector\Php53\Rector\Variable\ReplaceHttpServerVarsByServerRector;

use Rector\Php54\Rector\Array_\LongArrayToShortArrayRector;
use Rector\Php54\Rector\Break_\RemoveZeroBreakContinueRector;
use Rector\Php54\Rector\FuncCall\RemoveReferenceFromCallRector;
use Rector\Renaming\Rector\FuncCall\RenameFunctionRector;

use Rector\Php55\Rector\Class_\ClassConstantToSelfClassRector;
use Rector\Php55\Rector\ClassConstFetch\StaticToSelfOnFinalClassRector;
use Rector\Php55\Rector\FuncCall\GetCalledClassToSelfClassRector;
use Rector\Php55\Rector\FuncCall\GetCalledClassToStaticClassRector;
use Rector\Php55\Rector\FuncCall\PregReplaceEModifierRector;
use Rector\Php55\Rector\String_\StringClassNameToClassConstantRector;

use Rector\Php56\Rector\FuncCall\PowToExpRector;

use Rector\Php70\Rector\Assign\ListSplitStringRector;
use Rector\Php70\Rector\Assign\ListSwapArrayOrderRector;
use Rector\Php70\Rector\Break_\BreakNotInLoopOrSwitchToReturnRector;
use Rector\Php70\Rector\ClassMethod\Php4ConstructorRector;
use Rector\Php70\Rector\FuncCall\CallUserMethodRector;
use Rector\Php70\Rector\FuncCall\EregToPregMatchRector;
use Rector\Php70\Rector\FuncCall\MultiDirnameRector;
use Rector\Php70\Rector\FuncCall\RandomFunctionRector;
use Rector\Php70\Rector\FuncCall\RenameMktimeWithoutArgsToTimeRector;
use Rector\Php70\Rector\FunctionLike\ExceptionHandlerTypehintRector;
use Rector\Php70\Rector\If_\IfToSpaceshipRector;
use Rector\Php70\Rector\List_\EmptyListRector;
use Rector\Php70\Rector\MethodCall\ThisCallOnStaticMethodToStaticCallRector;
use Rector\Php70\Rector\StaticCall\StaticCallOnNonStaticToInstanceCallRector;
use Rector\Php70\Rector\StmtsAwareInterface\IfIssetToCoalescingRector;
use Rector\Php70\Rector\Switch_\ReduceMultipleDefaultSwitchRector;
use Rector\Php70\Rector\Ternary\TernaryToNullCoalescingRector;
use Rector\Php70\Rector\Ternary\TernaryToSpaceshipRector;
use Rector\Php70\Rector\Variable\WrapVariableVariableNameInCurlyBracesRector;

use Rector\Php71\Rector\Assign\AssignArrayToStringRector;
use Rector\Php71\Rector\BinaryOp\BinaryOpBetweenNumberAndStringRector;
use Rector\Php71\Rector\BooleanOr\IsIterableRector;
use Rector\Php71\Rector\ClassConst\PublicConstantVisibilityRector;
use Rector\Php71\Rector\FuncCall\RemoveExtraParametersRector;
use Rector\Php71\Rector\List_\ListToArrayDestructRector;
use Rector\Php71\Rector\TryCatch\MultiExceptionCatchRector;

use Rector\Php72\Rector\Assign\ListEachRector;
use Rector\Php72\Rector\Assign\ReplaceEachAssignmentWithKeyCurrentRector;
use Rector\Php72\Rector\FuncCall\CreateFunctionToAnonymousFunctionRector;
use Rector\Php72\Rector\FuncCall\GetClassOnNullRector;
use Rector\Php72\Rector\FuncCall\ParseStrWithResultArgumentRector;
use Rector\Php72\Rector\FuncCall\StringifyDefineRector;
use Rector\Php72\Rector\FuncCall\StringsAssertNakedRector;
use Rector\Php72\Rector\Unset_\UnsetCastRector;
use Rector\Php72\Rector\While_\WhileEachToForeachRector;

use Rector\Php73\Rector\BooleanOr\IsCountableRector;
use Rector\Php73\Rector\ConstFetch\SensitiveConstantNameRector;
use Rector\Php73\Rector\FuncCall\ArrayKeyFirstLastRector;
use Rector\Php73\Rector\FuncCall\RegexDashEscapeRector;
use Rector\Php73\Rector\FuncCall\SensitiveDefineRector;
use Rector\Php73\Rector\FuncCall\SetCookieRector;
use Rector\Php73\Rector\FuncCall\StringifyStrNeedlesRector;
use Rector\Php73\Rector\String_\SensitiveHereNowDocRector;

use Rector\Php74\Rector\ArrayDimFetch\CurlyToSquareBracketArrayStringRector;
use Rector\Php74\Rector\Assign\NullCoalescingOperatorRector;
use Rector\Php74\Rector\Closure\ClosureToArrowFunctionRector;
use Rector\Php74\Rector\Double\RealToFloatTypeCastRector;
use Rector\Php74\Rector\FuncCall\ArrayKeyExistsOnPropertyRector;
use Rector\Php74\Rector\FuncCall\FilterVarToAddSlashesRector;
use Rector\Php74\Rector\FuncCall\HebrevcToNl2brHebrevRector;
use Rector\Php74\Rector\FuncCall\MbStrrposEncodingArgumentPositionRector;
use Rector\Php74\Rector\FuncCall\MoneyFormatToNumberFormatRector;
use Rector\Php74\Rector\FuncCall\RestoreIncludePathToIniRestoreRector;
use Rector\Php74\Rector\Property\RestoreDefaultNullToNullableTypePropertyRector;
use Rector\Php74\Rector\StaticCall\ExportToReflectionFunctionRector;
use Rector\Php74\Rector\Ternary\ParenthesizeNestedTernaryRector;

use Rector\Arguments\Rector\ClassMethod\ArgumentAdderRector;
use Rector\Arguments\Rector\FuncCall\FunctionArgumentDefaultValueReplacerRector;
use Rector\Arguments\ValueObject\ArgumentAdder;
use Rector\Arguments\ValueObject\ReplaceFuncCallArgumentDefaultValue;
use Rector\CodeQuality\Rector\ClassMethod\OptionalParametersAfterRequiredRector;
use Rector\CodingStyle\Rector\FuncCall\ConsistentImplodeRector;
use Rector\DeadCode\Rector\StaticCall\RemoveParentCallWithoutParentRector;
use Rector\Php80\Rector\Catch_\RemoveUnusedVariableInCatchRector;
use Rector\Php80\Rector\Class_\ClassPropertyAssignToConstructorPromotionRector;
use Rector\Php80\Rector\Class_\StringableForToStringRector;
use Rector\Php80\Rector\ClassConstFetch\ClassOnThisVariableObjectRector;
use Rector\Php80\Rector\ClassMethod\AddParamBasedOnParentClassMethodRector;
use Rector\Php80\Rector\ClassMethod\FinalPrivateToPrivateVisibilityRector;
use Rector\Php80\Rector\ClassMethod\SetStateToStaticRector;
use Rector\Php80\Rector\FuncCall\ClassOnObjectRector;
use Rector\Php80\Rector\FunctionLike\MixedTypeRector;
use Rector\Php80\Rector\Identical\StrEndsWithRector;
use Rector\Php80\Rector\Identical\StrStartsWithRector;
use Rector\Php80\Rector\NotIdentical\StrContainsRector;
use Rector\Php80\Rector\Switch_\ChangeSwitchToMatchRector;
use Rector\Php80\Rector\Ternary\GetDebugTypeRector;
use Rector\Transform\Rector\StaticCall\StaticCallToFuncCallRector;
use Rector\Transform\ValueObject\StaticCallToFuncCall;

use Rector\Php81\Rector\Array_\FirstClassCallableRector;
use Rector\Php81\Rector\Class_\MyCLabsClassToEnumRector;
use Rector\Php81\Rector\Class_\SpatieEnumClassToEnumRector;
use Rector\Php81\Rector\ClassMethod\NewInInitializerRector;
use Rector\Php81\Rector\FuncCall\NullToStrictStringFuncCallArgRector;
use Rector\Php81\Rector\MethodCall\MyCLabsMethodCallToEnumConstRector;
use Rector\Php81\Rector\MethodCall\SpatieEnumMethodCallToEnumConstRector;
use Rector\Php81\Rector\Property\ReadOnlyPropertyRector;
use Rector\TypeDeclaration\Rector\ClassMethod\ReturnNeverTypeRector;

use Rector\Php82\Rector\Class_\ReadOnlyClassRector;
use Rector\Php82\Rector\Encapsed\VariableInStringInterpolationFixerRector;
use Rector\Php82\Rector\FuncCall\Utf8DecodeEncodeToMbConvertEncodingRector;
use Rector\Php82\Rector\New_\FilesystemIteratorSkipDotsRector;

use Rector\Php83\Rector\ClassConst\AddTypeToConstRector;
use Rector\Php83\Rector\ClassMethod\AddOverrideAttributeToOverriddenMethodsRector;
use Rector\Php83\Rector\FuncCall\CombineHostPortLdapUriRector;
use Rector\Php83\Rector\FuncCall\RemoveGetClassGetParentClassNoArgsRector;

use Rector\Php84\Rector\Param\ExplicitNullableParamTypeRector;

return static function (RectorConfig $rectorConfig): void {

    $path = __DIR__ . '/../../modules';
    $excludedDirs = [
        'ACL', 'ACLActions', 'ACLRoles',
        'AM_ProjectTemplates', 'AM_TaskTemplates',
        'AOBH_BusinessHours',
        'AOD_Index', 'AOD_IndexEvent',
        'AOK_KnowledgeBase', 'AOK_Knowledge_Base_Categories',
        'AOP_Case_Events', 'AOP_Case_Updates',
        'AOR_Charts', 'AOR_Conditions', 'AOR_Fields', 'AOR_Reports', 'AOR_Scheduled_Reports',
        'AOS_Contracts', 'AOS_Invoices', 'AOS_Line_Item_Groups', 'AOS_PDF_Templates', 'AOS_Product_Categories', 'AOS_Products', 'AOS_Products_Quotes', 'AOS_Quotes',
        'AOW_Actions', 'AOW_Conditions', 'AOW_Processed', 'AOW_WorkFlow',
        'Accounts', 'Activities', 'Administration', 'Alerts', 'Audit', 'Bugs',
        'Calendar', 'Calls', 'Calls_Reschedule',
        'CampaignLog', 'CampaignTrackers', 'Campaigns',
        'Cases', 'Charts', 'Configurator', 'Connectors', 'Contacts', 'Currencies',
        'DHA_PlantillasDocumentos', 'Delegates', 'DocumentRevisions', 'Documents',
        'DynamicFields', 'EAPM',
        'EmailAddresses', 'EmailMan', 'EmailMarketing', 'EmailTemplates', 'EmailText', 'Emails',
        'Employees', 'ExternalOAuthConnection', 'ExternalOAuthProvider',
        'FP_Event_Locations', 'FP_events',
        'Favorites', 'Groups', 'Help', 'History', 'Home', 'Import',
        'InboundEmail', 'JAccount', 'KReports', 'LabelEditor', 'Leads',
        'LoginAttempts', 'MailMerge', 'Meetings', 'MergeRecords', 'ModuleBuilder',
        'MySettings', 'Notes',
        'OAuth2Clients', 'OAuth2Tokens', 'OAuthKeys', 'OAuthTokens',
        'Opportunities', 'OptimisticLock', 'OutboundEmailAccounts',
        'Project', 'ProjectTask', 'ProspectLists',
        'Prospects', 'Relationships', 'Releases',
        'Reminders', 'Reminders_Invitees',
        'ResourceCalendar', 'Roles', 'SavedSearch',
        'Schedulers', 'SchedulersJobs', 'SecurityGroups',
        'SharedSecurityRules', 'SharedSecurityRulesActions', 'SharedSecurityRulesConditions',
        'Spots', 'Studio', 'SugarFeed',
        'SurveyQuestionOptions', 'SurveyQuestionResponses', 'SurveyQuestions', 'SurveyResponses', 'Surveys',
        'Tasks', 'TemplateSectionLine', 'Trackers', 'UpgradeWizard', 'UserPreferences', 'Users', 'iCals',
        'jjwg_Address_Cache', 'jjwg_Areas', 'jjwg_Maps', 'jjwg_Markers', 'vCals',
        'stic_Accounts_Relationships','stic_Assessments','stic_Attendances','stic_Bookings',
        'stic_Bookings_Calendar','stic_Bookings_Places_Calendar','stic_Centers','stic_Contacts_Relationships','stic_Custom_View_Actions','stic_Custom_View_Conditions',
        'stic_Custom_View_Customizations','stic_Custom_Views','stic_Events','stic_Families','stic_FollowUps','stic_Goals',
        'stic_Grants','stic_Group_Opportunities','stic_Import_Validation','stic_Incorpora','stic_Incorpora_Locations','stic_Job_Applications',
        'stic_Job_Offers','stic_Journal','stic_Medication','stic_Medication_Log','stic_Payment_Commitments','stic_Payments',
        'stic_Personal_Environment','stic_Prescription','stic_Registrations','stic_Remittances','stic_Resources','stic_Security_Groups_Rules',
        'stic_Sepe_Actions','stic_Sepe_Files','stic_Sepe_Incidents','stic_Sessions','stic_Settings','stic_Skills','stic_Time_Tracker',
        'stic_Training','stic_Validation_Actions','stic_Validation_Results','stic_Web_Forms','stic_Work_Calendar','stic_Work_Experience',
    ];

    // Get all dirs in 'modules' except $excludedDirs dirs
    $directories = array_filter(scandir($path), function ($item) use ($path, $excludedDirs) {
        return $item !== '.' && $item !== '..' 
               && is_dir($path . '/' . $item)
               && !in_array($item, $excludedDirs);
    });

    // Get all path for dirs in 'modules'
    $directories = array_map(function ($item) use ($path) {
        return $path . '/' . $item;
    }, $directories);

    // Add other directories
    $directories[] = __DIR__ . '/../../custom';
  
    // Path to apply changes
    $rectorConfig->paths($directories);

    // Cache for rector
    $cacheDirectory = __DIR__ . '/../../cache/rector_cached_files';
    $rectorConfig->cacheDirectory($cacheDirectory);

    // php52
    $rectorConfig->rules([
        VarToPublicPropertyRector::class, 
        ContinueToBreakInSwitchRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(
        RemoveFuncCallArgRector::class, [new RemoveFuncCallArg('ldap_first_attribute', 2),]
    );

    // php53
    $rectorConfig->rules([
        // TernaryToElvisRector::class,
        // DirNameFileConstantToDirConstantRector::class, 
        ReplaceHttpServerVarsByServerRector::class
    ]);

    // php54
    $rectorConfig->rules([
        // LongArrayToShortArrayRector::class, 
        RemoveReferenceFromCallRector::class, 
        RemoveZeroBreakContinueRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(
        RenameFunctionRector::class, ['mysqli_param_count' => 'mysqli_stmt_param_count']
    );

    // php55
    $rectorConfig->rules([
        // StringClassNameToClassConstantRector::class, 
        // ClassConstantToSelfClassRector::class, 
        PregReplaceEModifierRector::class, 
        // GetCalledClassToSelfClassRector::class, 
        // GetCalledClassToStaticClassRector::class, 
        StaticToSelfOnFinalClassRector::class
    ]);

    // php56
    // $rectorConfig->rule(PowToExpRector::class);
    $rectorConfig->ruleWithConfiguration(
        RenameFunctionRector::class, [
            'mcrypt_generic_end' => 'mcrypt_generic_deinit', 
            'set_socket_blocking' => 'stream_set_blocking', 
            'ocibindbyname' => 'oci_bind_by_name', 
            'ocicancel' => 'oci_cancel', 
            'ocicolumnisnull' => 'oci_field_is_null', 
            'ocicolumnname' => 'oci_field_name', 
            'ocicolumnprecision' => 'oci_field_precision', 
            'ocicolumnscale' => 'oci_field_scale', 
            'ocicolumnsize' => 'oci_field_size', 
            'ocicolumntype' => 'oci_field_type', 
            'ocicolumntyperaw' => 'oci_field_type_raw', 
            'ocicommit' => 'oci_commit', 
            'ocidefinebyname' => 'oci_define_by_name', 
            'ocierror' => 'oci_error', 
            'ociexecute' => 'oci_execute', 
            'ocifetch' => 'oci_fetch', 
            'ocifetchstatement' => 'oci_fetch_all', 
            'ocifreecursor' => 'oci_free_statement', 
            'ocifreestatement' => 'oci_free_statement', 
            'ociinternaldebug' => 'oci_internal_debug', 
            'ocilogoff' => 'oci_close', 
            'ocilogon' => 'oci_connect', 
            'ocinewcollection' => 'oci_new_collection', 
            'ocinewcursor' => 'oci_new_cursor', 
            'ocinewdescriptor' => 'oci_new_descriptor', 
            'ocinlogon' => 'oci_new_connect', 
            'ocinumcols' => 'oci_num_fields', 
            'ociparse' => 'oci_parse', 
            'ociplogon' => 'oci_pconnect', 
            'ociresult' => 'oci_result', 
            'ocirollback' => 'oci_rollback', 
            'ocirowcount' => 'oci_num_rows', 
            'ociserverversion' => 'oci_server_version', 
            'ocisetprefetch' => 'oci_set_prefetch', 
            'ocistatementtype' => 'oci_statement_type'
        ]
    );

    // php70
    $rectorConfig->rules([
        Php4ConstructorRector::class,
        // TernaryToNullCoalescingRector::class,
        // RandomFunctionRector::class,
        // ExceptionHandlerTypehintRector::class,
        // MultiDirnameRector::class,
        ListSplitStringRector::class,
        EmptyListRector::class,
        // be careful, run this just once, since it can keep swapping order back and forth
        ListSwapArrayOrderRector::class,
        CallUserMethodRector::class,
        EregToPregMatchRector::class,
        ReduceMultipleDefaultSwitchRector::class,
        // TernaryToSpaceshipRector::class,
        // WrapVariableVariableNameInCurlyBracesRector::class,
        // IfToSpaceshipRector::class,
        StaticCallOnNonStaticToInstanceCallRector::class,
        ThisCallOnStaticMethodToStaticCallRector::class,
        BreakNotInLoopOrSwitchToReturnRector::class,
        RenameMktimeWithoutArgsToTimeRector::class,
        // IfIssetToCoalescingRector::class,
    ]);

    // php71
    $rectorConfig->rules([
        IsIterableRector::class, 
        // MultiExceptionCatchRector::class, 
        AssignArrayToStringRector::class, 
        // RemoveExtraParametersRector::class, 
        BinaryOpBetweenNumberAndStringRector::class, 
        // ListToArrayDestructRector::class, 
        // PublicConstantVisibilityRector::class
    ]);

    // php72
    $rectorConfig->rules([
        GetClassOnNullRector::class, 
        ParseStrWithResultArgumentRector::class, 
        StringsAssertNakedRector::class, 
        CreateFunctionToAnonymousFunctionRector::class, 
        StringifyDefineRector::class, 
        WhileEachToForeachRector::class, 
        ListEachRector::class, 
        ReplaceEachAssignmentWithKeyCurrentRector::class, 
        UnsetCastRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(RenameFunctionRector::class, [
        # and imagewbmp
        'jpeg2wbmp' => 'imagecreatefromjpeg',
        # or imagewbmp
        'png2wbmp' => 'imagecreatefrompng',
        #migration72.deprecated.gmp_random-function
        # http://php.net/manual/en/migration72.deprecated.php
        # or gmp_random_range
        'gmp_random' => 'gmp_random_bits',
        'read_exif_data' => 'exif_read_data',
    ]);

    // php73
    $rectorConfig->rules([
        // StringifyStrNeedlesRector::class, 
        RegexDashEscapeRector::class, 
        ContinueToBreakInSwitchRector::class, 
        // SetCookieRector::class, 
        // IsCountableRector::class, 
        // ArrayKeyFirstLastRector::class, 
        SensitiveDefineRector::class, 
        // SensitiveConstantNameRector::class, 
        SensitiveHereNowDocRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(RenameFunctionRector::class, [
        # https://wiki.php.net/rfc/deprecations_php_7_3
        'image2wbmp' => 'imagewbmp',
        'mbregex_encoding' => 'mb_regex_encoding',
        'mbereg' => 'mb_ereg',
        'mberegi' => 'mb_eregi',
        'mbereg_replace' => 'mb_ereg_replace',
        'mberegi_replace' => 'mb_eregi_replace',
        'mbsplit' => 'mb_split',
        'mbereg_match' => 'mb_ereg_match',
        'mbereg_search' => 'mb_ereg_search',
        'mbereg_search_pos' => 'mb_ereg_search_pos',
        'mbereg_search_regs' => 'mb_ereg_search_regs',
        'mbereg_search_init' => 'mb_ereg_search_init',
        'mbereg_search_getregs' => 'mb_ereg_search_getregs',
        'mbereg_search_getpos' => 'mb_ereg_search_getpos',
    ]);

    // php74
    $rectorConfig->rules([
        ArrayKeyExistsOnPropertyRector::class, 
        FilterVarToAddSlashesRector::class, 
        ExportToReflectionFunctionRector::class, 
        MbStrrposEncodingArgumentPositionRector::class,
        RealToFloatTypeCastRector::class, 
        // NullCoalescingOperatorRector::class, 
        // ClosureToArrowFunctionRector::class, 
        RestoreDefaultNullToNullableTypePropertyRector::class, 
        CurlyToSquareBracketArrayStringRector::class, 
        MoneyFormatToNumberFormatRector::class, 
        ParenthesizeNestedTernaryRector::class, 
        RestoreIncludePathToIniRestoreRector::class, 
        HebrevcToNl2brHebrevRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(RenameFunctionRector::class, [
        #the_real_type
        # https://wiki.php.net/rfc/deprecations_php_7_4
        'is_real' => 'is_float',
    ]);

    // php80
    $rectorConfig->rules([
        // StrContainsRector::class, 
        // StrStartsWithRector::class, 
        // StrEndsWithRector::class, 
        StringableForToStringRector::class, 
        ClassOnObjectRector::class, 
        // GetDebugTypeRector::class, 
        // RemoveUnusedVariableInCatchRector::class, 
        // ClassPropertyAssignToConstructorPromotionRector::class, 
        // ChangeSwitchToMatchRector::class, 
        // RemoveParentCallWithoutParentRector::class, 
        SetStateToStaticRector::class, 
        FinalPrivateToPrivateVisibilityRector::class, 
        AddParamBasedOnParentClassMethodRector::class, 
        MixedTypeRector::class, 
        ClassOnThisVariableObjectRector::class, 
        ConsistentImplodeRector::class
    ]);
    $rectorConfig->ruleWithConfiguration(
        StaticCallToFuncCallRector::class, [
            new StaticCallToFuncCall('Nette\\Utils\\Strings', 'startsWith', 'str_starts_with'), 
            new StaticCallToFuncCall('Nette\\Utils\\Strings', 'endsWith', 'str_ends_with'), 
            new StaticCallToFuncCall('Nette\\Utils\\Strings', 'contains', 'str_contains')
        ]
    );
    // nette\utils and Strings::replace()
    $rectorConfig->ruleWithConfiguration(
        ArgumentAdderRector::class, [
            new ArgumentAdder('Nette\\Utils\\Strings', 'replace', 2, 'replacement', '')
        ]
    );
    // @see https://php.watch/versions/8.0/pgsql-aliases-deprecated
    $rectorConfig->ruleWithConfiguration(
        RenameFunctionRector::class, [
            'pg_clientencoding' => 'pg_client_encoding', 
            'pg_cmdtuples' => 'pg_affected_rows', 
            'pg_errormessage' => 'pg_last_error', 
            'pg_fieldisnull' => 'pg_field_is_null', 
            'pg_fieldname' => 'pg_field_name', 
            'pg_fieldnum' => 'pg_field_num', 
            'pg_fieldprtlen' => 'pg_field_prtlen', 
            'pg_fieldsize' => 'pg_field_size', 
            'pg_fieldtype' => 'pg_field_type', 
            'pg_freeresult' => 'pg_free_result', 
            'pg_getlastoid' => 'pg_last_oid', 
            'pg_loclose' => 'pg_lo_close', 
            'pg_locreate' => 'pg_lo_create', 
            'pg_loexport' => 'pg_lo_export', 
            'pg_loimport' => 'pg_lo_import', 
            'pg_loopen' => 'pg_lo_open', 
            'pg_loread' => 'pg_lo_read', 
            'pg_loreadall' => 'pg_lo_read_all', 
            'pg_lounlink' => 'pg_lo_unlink', 
            'pg_lowrite' => 'pg_lo_write', 
            'pg_numfields' => 'pg_num_fields', 
            'pg_numrows' => 'pg_num_rows', 
            'pg_result' => 'pg_fetch_result', 
            'pg_setclientencoding' => 'pg_set_client_encoding'
        ]
    );
    // $rectorConfig->rule(OptionalParametersAfterRequiredRector::class);
    $rectorConfig->ruleWithConfiguration(
        FunctionArgumentDefaultValueReplacerRector::class, [
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, 'gte', 'ge'), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, 'lte', 'le'), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, '', '!='), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, '!', '!='), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, 'g', 'gt'), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, 'l', 'lt'), 
            new ReplaceFuncCallArgumentDefaultValue('version_compare', 2, 'n', 'ne'), 
            new ReplaceFuncCallArgumentDefaultValue('get_headers', 1, 0, \false), 
            new ReplaceFuncCallArgumentDefaultValue('get_headers', 1, 1, \true)
        ]
    );

    // php81
    $rectorConfig->rules([
        ReturnNeverTypeRector::class, 
        MyCLabsClassToEnumRector::class, 
        MyCLabsMethodCallToEnumConstRector::class, 
        // ReadOnlyPropertyRector::class, 
        SpatieEnumClassToEnumRector::class, 
        SpatieEnumMethodCallToEnumConstRector::class, 
        // NewInInitializerRector::class, 
        NullToStrictStringFuncCallArgRector::class, 
        FirstClassCallableRector::class
    ]);

    // php82
    $rectorConfig->rules([
        ReadOnlyClassRector::class, 
        Utf8DecodeEncodeToMbConvertEncodingRector::class, 
        FilesystemIteratorSkipDotsRector::class, 
        VariableInStringInterpolationFixerRector::class
    ]);

    // php83
    $rectorConfig->rules([
        // AddOverrideAttributeToOverriddenMethodsRector::class,
        // AddTypeToConstRector::class,
        CombineHostPortLdapUriRector::class,
        RemoveGetClassGetParentClassNoArgsRector::class
    ]);

    // php84
    $rectorConfig->rules([
        ExplicitNullableParamTypeRector::class
    ]);

    // Skip unnecessary Paths
    // $configure = $rectorConfig->configure();
    // $configure->withSkip([
    //     __DIR__ . '/src/SingleFile.php',
    //     __DIR__ . '/src/WholeDirectory',
    // ]);
};