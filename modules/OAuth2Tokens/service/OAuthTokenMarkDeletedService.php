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

class OAuthTokenMarkDeletedService
{
    public function run(): void
    {

        $db = $this->getDBManager();

        $threshold = $this->getOAuthTokenDeleteThreshold();
        $date = (new DateTime($threshold))->format('Y-m-d H:i:s');

        $query = "
            UPDATE oauth2tokens
            SET deleted = 1
            WHERE (
                (refresh_token_expires < '$date' AND access_token_expires < '$date')
                OR (token_is_revoked = 1 AND date_modified < '$date')
              )
              AND deleted = 0
         ";

        $db->query($query);
    }

    /**
     * @return DBManager|object
     */
    protected function getDBManager()
    {
        $db = DBManagerFactory::getInstance();
        if ($db === null) {
            $this->log('error', 'Database connection error');
            throw new RuntimeException('Database connection error');
        }
        return $db;
    }

    /**
     * @param string $level
     * @param string $message
     * @param string $section
     * @return void
     */
    protected function log(string $level, string $message, string $section = ''): void
    {
        $logMessage = '[OAuth2TokenMarkDeletedService]';
        if (!empty($section)) {
            $logMessage .= '[' . $section . ']';
        }

        $logMessage .= ' ' . $message;

        $GLOBALS['log']->$level($logMessage);
    }

    protected function getOAuthTokenDeleteThreshold(): string
    {
        $configurator = new Configurator();
        $configurator->loadConfig();

        if (!empty($configurator->config['oauth_token_delete_threshold']) && is_string($configurator->config['oauth_token_delete_threshold'])) {
            $value = strtolower(trim($configurator->config['oauth_token_delete_threshold']));
            if (preg_match('/^-\d+\s+days$/', $value)) {
                return $value;
            }
            $this->log('warning', sprintf('Invalid oauth_token_delete_threshold "%s", using default -7 days', $value), 'config');

        }

        return '-7 days';
    }

}