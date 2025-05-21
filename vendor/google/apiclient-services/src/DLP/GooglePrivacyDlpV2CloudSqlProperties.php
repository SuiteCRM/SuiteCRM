<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\DLP;

class GooglePrivacyDlpV2CloudSqlProperties extends \Google\Model
{
  protected $cloudSqlIamType = GooglePrivacyDlpV2CloudSqlIamCredential::class;
  protected $cloudSqlIamDataType = '';
  /**
   * @var string
   */
  public $connectionName;
  /**
   * @var string
   */
  public $databaseEngine;
  /**
   * @var int
   */
  public $maxConnections;
  protected $usernamePasswordType = GooglePrivacyDlpV2SecretManagerCredential::class;
  protected $usernamePasswordDataType = '';

  /**
   * @param GooglePrivacyDlpV2CloudSqlIamCredential
   */
  public function setCloudSqlIam(GooglePrivacyDlpV2CloudSqlIamCredential $cloudSqlIam)
  {
    $this->cloudSqlIam = $cloudSqlIam;
  }
  /**
   * @return GooglePrivacyDlpV2CloudSqlIamCredential
   */
  public function getCloudSqlIam()
  {
    return $this->cloudSqlIam;
  }
  /**
   * @param string
   */
  public function setConnectionName($connectionName)
  {
    $this->connectionName = $connectionName;
  }
  /**
   * @return string
   */
  public function getConnectionName()
  {
    return $this->connectionName;
  }
  /**
   * @param string
   */
  public function setDatabaseEngine($databaseEngine)
  {
    $this->databaseEngine = $databaseEngine;
  }
  /**
   * @return string
   */
  public function getDatabaseEngine()
  {
    return $this->databaseEngine;
  }
  /**
   * @param int
   */
  public function setMaxConnections($maxConnections)
  {
    $this->maxConnections = $maxConnections;
  }
  /**
   * @return int
   */
  public function getMaxConnections()
  {
    return $this->maxConnections;
  }
  /**
   * @param GooglePrivacyDlpV2SecretManagerCredential
   */
  public function setUsernamePassword(GooglePrivacyDlpV2SecretManagerCredential $usernamePassword)
  {
    $this->usernamePassword = $usernamePassword;
  }
  /**
   * @return GooglePrivacyDlpV2SecretManagerCredential
   */
  public function getUsernamePassword()
  {
    return $this->usernamePassword;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2CloudSqlProperties::class, 'Google_Service_DLP_GooglePrivacyDlpV2CloudSqlProperties');
