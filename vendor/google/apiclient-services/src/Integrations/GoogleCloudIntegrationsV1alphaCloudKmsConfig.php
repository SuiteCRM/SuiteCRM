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

class GoogleCloudIntegrationsV1alphaCloudKmsConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $key;
  /**
   * @var string
   */
  public $keyVersion;
  /**
   * @var string
   */
  public $kmsLocation;
  /**
   * @var string
   */
  public $kmsProjectId;
  /**
   * @var string
   */
  public $kmsRing;

  /**
   * @param string
   */
  public function setKey($key)
  {
    $this->key = $key;
  }
  /**
   * @return string
   */
  public function getKey()
  {
    return $this->key;
  }
  /**
   * @param string
   */
  public function setKeyVersion($keyVersion)
  {
    $this->keyVersion = $keyVersion;
  }
  /**
   * @return string
   */
  public function getKeyVersion()
  {
    return $this->keyVersion;
  }
  /**
   * @param string
   */
  public function setKmsLocation($kmsLocation)
  {
    $this->kmsLocation = $kmsLocation;
  }
  /**
   * @return string
   */
  public function getKmsLocation()
  {
    return $this->kmsLocation;
  }
  /**
   * @param string
   */
  public function setKmsProjectId($kmsProjectId)
  {
    $this->kmsProjectId = $kmsProjectId;
  }
  /**
   * @return string
   */
  public function getKmsProjectId()
  {
    return $this->kmsProjectId;
  }
  /**
   * @param string
   */
  public function setKmsRing($kmsRing)
  {
    $this->kmsRing = $kmsRing;
  }
  /**
   * @return string
   */
  public function getKmsRing()
  {
    return $this->kmsRing;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaCloudKmsConfig::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaCloudKmsConfig');
