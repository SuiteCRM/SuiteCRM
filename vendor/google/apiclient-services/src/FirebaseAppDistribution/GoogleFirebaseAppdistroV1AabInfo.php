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

namespace Google\Service\FirebaseAppDistribution;

class GoogleFirebaseAppdistroV1AabInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $integrationState;
  /**
   * @var string
   */
  public $name;
  protected $testCertificateType = GoogleFirebaseAppdistroV1TestCertificate::class;
  protected $testCertificateDataType = '';

  /**
   * @param string
   */
  public function setIntegrationState($integrationState)
  {
    $this->integrationState = $integrationState;
  }
  /**
   * @return string
   */
  public function getIntegrationState()
  {
    return $this->integrationState;
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
   * @param GoogleFirebaseAppdistroV1TestCertificate
   */
  public function setTestCertificate(GoogleFirebaseAppdistroV1TestCertificate $testCertificate)
  {
    $this->testCertificate = $testCertificate;
  }
  /**
   * @return GoogleFirebaseAppdistroV1TestCertificate
   */
  public function getTestCertificate()
  {
    return $this->testCertificate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirebaseAppdistroV1AabInfo::class, 'Google_Service_FirebaseAppDistribution_GoogleFirebaseAppdistroV1AabInfo');
