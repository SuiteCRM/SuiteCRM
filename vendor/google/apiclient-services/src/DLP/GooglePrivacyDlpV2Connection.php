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

class GooglePrivacyDlpV2Connection extends \Google\Collection
{
  protected $collection_key = 'errors';
  protected $cloudSqlType = GooglePrivacyDlpV2CloudSqlProperties::class;
  protected $cloudSqlDataType = '';
  protected $errorsType = GooglePrivacyDlpV2Error::class;
  protected $errorsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;

  /**
   * @param GooglePrivacyDlpV2CloudSqlProperties
   */
  public function setCloudSql(GooglePrivacyDlpV2CloudSqlProperties $cloudSql)
  {
    $this->cloudSql = $cloudSql;
  }
  /**
   * @return GooglePrivacyDlpV2CloudSqlProperties
   */
  public function getCloudSql()
  {
    return $this->cloudSql;
  }
  /**
   * @param GooglePrivacyDlpV2Error[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return GooglePrivacyDlpV2Error[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2Connection::class, 'Google_Service_DLP_GooglePrivacyDlpV2Connection');
