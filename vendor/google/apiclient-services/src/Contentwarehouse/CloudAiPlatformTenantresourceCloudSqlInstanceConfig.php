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

namespace Google\Service\Contentwarehouse;

class CloudAiPlatformTenantresourceCloudSqlInstanceConfig extends \Google\Collection
{
  protected $collection_key = 'mdbRolesForCorpAccess';
  /**
   * @var string
   */
  public $cloudSqlInstanceConnectionName;
  /**
   * @var string
   */
  public $cloudSqlInstanceName;
  /**
   * @var string
   */
  public $kmsKeyReference;
  /**
   * @var string[]
   */
  public $mdbRolesForCorpAccess;
  /**
   * @var string
   */
  public $slmInstanceName;
  /**
   * @var string
   */
  public $slmInstanceTemplate;
  /**
   * @var string
   */
  public $slmInstanceType;

  /**
   * @param string
   */
  public function setCloudSqlInstanceConnectionName($cloudSqlInstanceConnectionName)
  {
    $this->cloudSqlInstanceConnectionName = $cloudSqlInstanceConnectionName;
  }
  /**
   * @return string
   */
  public function getCloudSqlInstanceConnectionName()
  {
    return $this->cloudSqlInstanceConnectionName;
  }
  /**
   * @param string
   */
  public function setCloudSqlInstanceName($cloudSqlInstanceName)
  {
    $this->cloudSqlInstanceName = $cloudSqlInstanceName;
  }
  /**
   * @return string
   */
  public function getCloudSqlInstanceName()
  {
    return $this->cloudSqlInstanceName;
  }
  /**
   * @param string
   */
  public function setKmsKeyReference($kmsKeyReference)
  {
    $this->kmsKeyReference = $kmsKeyReference;
  }
  /**
   * @return string
   */
  public function getKmsKeyReference()
  {
    return $this->kmsKeyReference;
  }
  /**
   * @param string[]
   */
  public function setMdbRolesForCorpAccess($mdbRolesForCorpAccess)
  {
    $this->mdbRolesForCorpAccess = $mdbRolesForCorpAccess;
  }
  /**
   * @return string[]
   */
  public function getMdbRolesForCorpAccess()
  {
    return $this->mdbRolesForCorpAccess;
  }
  /**
   * @param string
   */
  public function setSlmInstanceName($slmInstanceName)
  {
    $this->slmInstanceName = $slmInstanceName;
  }
  /**
   * @return string
   */
  public function getSlmInstanceName()
  {
    return $this->slmInstanceName;
  }
  /**
   * @param string
   */
  public function setSlmInstanceTemplate($slmInstanceTemplate)
  {
    $this->slmInstanceTemplate = $slmInstanceTemplate;
  }
  /**
   * @return string
   */
  public function getSlmInstanceTemplate()
  {
    return $this->slmInstanceTemplate;
  }
  /**
   * @param string
   */
  public function setSlmInstanceType($slmInstanceType)
  {
    $this->slmInstanceType = $slmInstanceType;
  }
  /**
   * @return string
   */
  public function getSlmInstanceType()
  {
    return $this->slmInstanceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiPlatformTenantresourceCloudSqlInstanceConfig::class, 'Google_Service_Contentwarehouse_CloudAiPlatformTenantresourceCloudSqlInstanceConfig');
