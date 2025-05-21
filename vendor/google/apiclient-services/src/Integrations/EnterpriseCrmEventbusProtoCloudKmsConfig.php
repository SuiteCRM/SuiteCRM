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

namespace Google\Service\Integrations;

class EnterpriseCrmEventbusProtoCloudKmsConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $gcpProjectId;
  /**
   * @var string
   */
  public $keyName;
  /**
   * @var string
   */
  public $keyRingName;
  /**
   * @var string
   */
  public $keyVersionName;
  /**
   * @var string
   */
  public $locationName;
  /**
   * @var string
   */
  public $serviceAccount;

  /**
   * @param string
   */
  public function setGcpProjectId($gcpProjectId)
  {
    $this->gcpProjectId = $gcpProjectId;
  }
  /**
   * @return string
   */
  public function getGcpProjectId()
  {
    return $this->gcpProjectId;
  }
  /**
   * @param string
   */
  public function setKeyName($keyName)
  {
    $this->keyName = $keyName;
  }
  /**
   * @return string
   */
  public function getKeyName()
  {
    return $this->keyName;
  }
  /**
   * @param string
   */
  public function setKeyRingName($keyRingName)
  {
    $this->keyRingName = $keyRingName;
  }
  /**
   * @return string
   */
  public function getKeyRingName()
  {
    return $this->keyRingName;
  }
  /**
   * @param string
   */
  public function setKeyVersionName($keyVersionName)
  {
    $this->keyVersionName = $keyVersionName;
  }
  /**
   * @return string
   */
  public function getKeyVersionName()
  {
    return $this->keyVersionName;
  }
  /**
   * @param string
   */
  public function setLocationName($locationName)
  {
    $this->locationName = $locationName;
  }
  /**
   * @return string
   */
  public function getLocationName()
  {
    return $this->locationName;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseCrmEventbusProtoCloudKmsConfig::class, 'Google_Service_Integrations_EnterpriseCrmEventbusProtoCloudKmsConfig');
