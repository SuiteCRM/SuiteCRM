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

class CloudAiPlatformTenantresourceGcsBucketConfig extends \Google\Collection
{
  protected $collection_key = 'viewers';
  /**
   * @var string[]
   */
  public $admins;
  /**
   * @var string
   */
  public $bucketName;
  /**
   * @var string
   */
  public $entityName;
  /**
   * @var string
   */
  public $kmsKeyReference;
  /**
   * @var int
   */
  public $ttlDays;
  /**
   * @var string[]
   */
  public $viewers;

  /**
   * @param string[]
   */
  public function setAdmins($admins)
  {
    $this->admins = $admins;
  }
  /**
   * @return string[]
   */
  public function getAdmins()
  {
    return $this->admins;
  }
  /**
   * @param string
   */
  public function setBucketName($bucketName)
  {
    $this->bucketName = $bucketName;
  }
  /**
   * @return string
   */
  public function getBucketName()
  {
    return $this->bucketName;
  }
  /**
   * @param string
   */
  public function setEntityName($entityName)
  {
    $this->entityName = $entityName;
  }
  /**
   * @return string
   */
  public function getEntityName()
  {
    return $this->entityName;
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
   * @param int
   */
  public function setTtlDays($ttlDays)
  {
    $this->ttlDays = $ttlDays;
  }
  /**
   * @return int
   */
  public function getTtlDays()
  {
    return $this->ttlDays;
  }
  /**
   * @param string[]
   */
  public function setViewers($viewers)
  {
    $this->viewers = $viewers;
  }
  /**
   * @return string[]
   */
  public function getViewers()
  {
    return $this->viewers;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiPlatformTenantresourceGcsBucketConfig::class, 'Google_Service_Contentwarehouse_CloudAiPlatformTenantresourceGcsBucketConfig');
