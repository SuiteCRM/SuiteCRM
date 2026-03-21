<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * @property CalendarAccount $bean
 */
#[AllowDynamicProperties]
class CalendarAccountController extends SugarController
{

    public function action_EditView(): void
    {
        $this->view = 'edit';

        if (empty($_REQUEST['type'])) {
            $_REQUEST['type'] = 'personal';
        }

        $this->bean->type = $_REQUEST['type'];
    }

    public function action_save(): void
    {
        try {
            $this->bean->save(!empty($this->bean->notify_on_save));
        }catch (Throwable $e) {
            SugarApplication::appendErrorMessage($e->getMessage());
            $recordParam = !empty($this->bean->id) ? "&record={$this->bean->id}" : '';
            SugarApplication::redirect("index.php?module={$this->bean->module_name}&action=EditView{$recordParam}");
        }
    }

    /**
     * Retrieves the authentication method for a specified calendar provider.
     *
     * This action fetches the provider's authentication method based on the given source.
     * Validates the source parameter and returns the corresponding authentication method
     * (oauth2, basic, api_key) for the calendar provider.
     *
     * @return void Sends JSON response with auth method data or error
     */
    public function action_getProviderAuthMethod(): void
    {
        global $log;
        $this->view = 'ajax';

        $source = $_REQUEST['source'] ?? '';

        try {
            require_once 'include/CalendarSync/CalendarSync.php';
            $authMethod = CalendarSync::getInstance()->getProviderAuthMethodWithValidation($source);

            $log->info("[CalendarAccountController] Auth method retrieved for source: $source ($authMethod)");

            $response['data'] = [
                'auth_method' => $authMethod,
                'source' => $source
            ];

            $this->sendJsonResponse($response);
        } catch (Exception $e) {
            [$response, $httpCode] = $this->handleApiException($e, 'auth_method', ['source' => $source]);
            $this->sendJsonResponse($response, $httpCode);
        }
    }

    /**
     * Tests calendar provider connection and returns detailed results.
     *
     * This action sets up a calendar account from request parameters, validates the
     * provider configuration, and tests the connection. Returns comprehensive data
     * about the test results including available calendars and account status.
     *
     * Supports all authentication methods: OAuth2, Basic Auth, and API Key.
     *
     * @return void Sends JSON response with connection test results or error
     */
    public function action_testConnection(): void
    {
        global $log, $timedate;
        $this->view = 'ajax';

        $source = $_REQUEST['source'] ?? '';

        try {
            $calendarAccount = $this->setupCalendarAccountFromRequest($source, $_REQUEST);

            require_once 'include/CalendarSync/CalendarSync.php';
            $testResult = CalendarSync::getInstance()->testProviderConnectionWithValidation($calendarAccount);

            if (!$testResult->isSuccessful()) {
                throw new RuntimeException('Connection test failed. ' . $testResult->getErrorMessage());
            }

            $log->info("[CalendarAccountController] Connection test successful for source: $source calendarAccount: $calendarAccount->id");

            $externalCalendarId = $testResult->getExternalCalendarId();
            $duplicateAccountInfo = null;

            if (!empty($externalCalendarId)) {
                $existingAccount = CalendarSync::getInstance()->findDuplicateCalendarAccount(
                    $externalCalendarId,
                    $calendarAccount->id
                );

                if ($existingAccount) {
                    $duplicateAccountInfo = [
                        'account_id' => $existingAccount->id,
                        'account_name' => $existingAccount->name,
                        'source' => $existingAccount->source
                    ];
                    $log->warn("[CalendarAccountController] Duplicate calendar detected - externalCalendarId: $externalCalendarId, existingAccount: $existingAccount->id");
                }
            }

            $currentTimeDb = $timedate->nowDb();

            $response['data'] = [
                'source' => $source,
                'connection_id' => $calendarAccount->oauth_connection_id,
                'test_result' => $testResult->toArray(),
                'calendar_account_id' => $calendarAccount->id ?? null,
                'calendar_account_name' => $calendarAccount->name ?? null,
                'has_oauth_connection' => !empty($calendarAccount->oauth_connection_id),
                'has_username' => !empty($calendarAccount->username),
                'has_server_url' => !empty($calendarAccount->server_url),
                'has_api_key' => !empty($calendarAccount->api_key),
                'has_api_endpoint' => !empty($calendarAccount->api_endpoint),
                'test_timestamp' => $currentTimeDb,
                'success' => $testResult->isSuccessful(),
                'error_message' => $testResult->getErrorMessage(),
                'external_calendar_id' => $externalCalendarId,
                'duplicate_account' => $duplicateAccountInfo,
            ];

            $this->sendJsonResponse($response);
        } catch (Exception $e) {
            [$response, $httpCode] = $this->handleApiException($e, 'connection_test', ['source' => $source]);
            $this->sendJsonResponse($response, $httpCode);
        }
    }

    /**
     * Triggers calendar synchronization for a specific calendar account.
     *
     * This action validates the account exists and has an active connection, then
     * performs a manual synchronization for the specified calendar account.
     * Returns confirmation of successful synchronization with account details.
     *
     * @return void Sends JSON response with sync results or error
     */
    public function action_syncCalendarAccount(): void
    {
        global $log, $timedate;
        $this->view = 'ajax';

        $recordId = $_REQUEST['record'] ?? '';

        try {
            require_once 'include/CalendarSync/CalendarSync.php';
            CalendarSync::getInstance()->syncAllMeetingsOfCalendarAccount($recordId);

            $log->info("[CalendarAccountController] Manual sync triggered for account: $recordId");

            $calendarAccount = BeanFactory::getBean('CalendarAccount', $recordId);

            $response['message'] = 'Calendar synchronization completed successfully';
            $response['data'] = [
                'account_id' => $recordId,
                'account_name' => $calendarAccount->name ?? '',
            ];

            $this->sendJsonResponse($response);
        } catch (Exception $e) {
            [$response, $httpCode] = $this->handleApiException($e, 'sync', ['recordId' => $recordId]);
            $this->sendJsonResponse($response, $httpCode);
        }
    }

    /**
     * Sets up a calendar account from request parameters.
     *
     * This method creates or loads an existing calendar account and populates it
     * with authentication credentials and configuration from the request data.
     * Handles OAuth connections, basic auth credentials, and API key configurations.
     *
     * @param string $source Calendar source identifier (google, outlook, etc.)
     * @param array $request Request parameters containing account configuration
     * @return CalendarAccount Configured calendar account ready for use
     */
    protected function setupCalendarAccountFromRequest(string $source, array $request): CalendarAccount
    {
        /** @var CalendarAccount|false $calendarAccount */
        $calendarAccount = BeanFactory::newBean('CalendarAccount');

        $calendarAccountId = $request['calendar_account_id'] ?? '';
        if (!empty($calendarAccountId)) {
            $existingAccount = BeanFactory::getBean('CalendarAccount', $calendarAccountId);
            if ($existingAccount) {
                $calendarAccount = $existingAccount;
            }
        }

        $calendarAccount->source = $source;

        if (!$calendarAccount->id) {
            $calendarAccount->id = 'uncreated_calendar_account';
        }

        if (!$calendarAccount->calendar_user_id) {
            $calendarAccount->calendar_user_id = 'unlinked_calendar_account';
        }

        if (!empty($request['oauth_connection_id'])) {
            $calendarAccount->oauth_connection_id = $request['oauth_connection_id'];
        }

        if (!empty($request['username'])) {
            $calendarAccount->username = $request['username'];
        }
        if (!empty($request['password'])) {
            $calendarAccount->password = $request['password'];
        }
        if (!empty($request['server_url'])) {
            $calendarAccount->server_url = $request['server_url'];
        }

        if (!empty($request['api_key'])) {
            $calendarAccount->api_key = $request['api_key'];
        }
        if (!empty($request['api_endpoint'])) {
            $calendarAccount->api_endpoint = $request['api_endpoint'];
        }

        return $calendarAccount;
    }

    /**
     * Handles exceptions and generates standardized error responses.
     *
     * This method maps exception types to appropriate HTTP status codes and error codes
     * based on the operation context. Handles logging for internal server errors and
     * generates consistent error response format across all API endpoints.
     *
     * @param Exception $e The caught exception from the facade layer
     * @param string $context Operation context ('auth_method', 'connection_test', 'sync')
     * @param array $contextData Additional data for logging and error code determination
     * @return array Tuple containing [response array, httpCode]
     */
    protected function handleApiException(Exception $e, string $context, array $contextData = []): array
    {
        global $log;

        $response = ['message' => $e->getMessage()];

        $httpCode = match (get_class($e)) {
            'InvalidArgumentException' => 400,
            'RuntimeException' => match ($context) {
                'auth_method' => 404,
                'connection_test' => 422,
                'sync' => !empty($contextData['recordId']) ? 422 : 404,
                default => 500
            },
            default => 500
        };

        if ($httpCode !== 500) {
            $response['error_code'] = match ($context) {
                'auth_method' => match (get_class($e)) {
                    'InvalidArgumentException' => 'MISSING_SOURCE',
                    'RuntimeException' => 'PROVIDER_NOT_FOUND',
                },
                'connection_test' => match (get_class($e)) {
                    'InvalidArgumentException' => !empty($contextData['source']) ? 'INVALID_ARGUMENT' : 'MISSING_SOURCE',
                    'RuntimeException' => 'CONNECTION_TEST_FAILED',
                },
                'sync' => match (get_class($e)) {
                    'InvalidArgumentException' => 'MISSING_RECORD_ID',
                    'RuntimeException' => !empty($contextData['recordId']) ? 'CONNECTION_NOT_ACTIVE' : 'RECORD_NOT_FOUND',
                },
                default => 'UNKNOWN_ERROR'
            };
        } else {
            $response['error_code'] = 'INTERNAL_ERROR';
        }

        if ($httpCode === 500) {
            $contextStr = match ($context) {
                'auth_method' => "Auth method error for source {$contextData['source']}",
                'connection_test' => "Connection test error",
                'sync' => "Sync error for record {$contextData['recordId']}",
                default => "Unknown error in context $context"
            };
            $log->error("[CalendarAccountController] $contextStr: " . $e->getMessage());
        }

        return [$response, $httpCode];
    }

    /**
     * Sends JSON responses with consistent formatting and status handling.
     *
     * This method handles HTTP status code setting, automatic status field assignment
     * based on HTTP code, proper JSON encoding with error handling, and output buffer
     * management for clean response delivery.
     *
     * @param array $response Response data to be JSON encoded
     * @param int|null $httpCode HTTP status code (defaults to 200 if null)
     * @return void Outputs JSON response directly to browser
     */
    protected function sendJsonResponse(array $response, ?int $httpCode = 200): void
    {
        $httpCode = $httpCode ?? 200;

        if ($httpCode !== 200) {
            http_response_code($httpCode);
        }

        if (!isset($response['status'])) {
            $response['status'] = $httpCode === 200 ? 'success' : 'error';
        }

        ob_clean();
        header('Content-Type: application/json');
        try {
            echo json_encode($response, JSON_THROW_ON_ERROR);
        } catch (JsonException $e) {
            echo '{"error":"Internal server error","message":"' . addslashes($e->getMessage() ?? '') . '"}';
        }
    }

}
