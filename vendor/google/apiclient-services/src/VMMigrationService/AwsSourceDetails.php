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

namespace Google\Service\VMMigrationService;

class AwsSourceDetails extends \Google\Collection
{
  protected $collection_key = 'inventoryTagList';
  protected $accessKeyCredsType = AccessKeyCredentials::class;
  protected $accessKeyCredsDataType = '';
  /**
   * @var string
   */
  public $awsRegion;
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string[]
   */
  public $inventorySecurityGroupNames;
  protected $inventoryTagListType = Tag::class;
  protected $inventoryTagListDataType = 'array';
  /**
   * @var string[]
   */
  public $migrationResourcesUserTags;
  /**
   * @var string
   */
  public $publicIp;
  /**
   * @var string
   */
  public $state;

  /**
   * @param AccessKeyCredentials
   */
  public function setAccessKeyCreds(AccessKeyCredentials $accessKeyCreds)
  {
    $this->accessKeyCreds = $accessKeyCreds;
  }
  /**
   * @return AccessKeyCredentials
   */
  public function getAccessKeyCreds()
  {
    return $this->accessKeyCreds;
  }
  /**
   * @param string
   */
  public function setAwsRegion($awsRegion)
  {
    $this->awsRegion = $awsRegion;
  }
  /**
   * @return string
   */
  public function getAwsRegion()
  {
    return $this->awsRegion;
  }
  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string[]
   */
  public function setInventorySecurityGroupNames($inventorySecurityGroupNames)
  {
    $this->inventorySecurityGroupNames = $inventorySecurityGroupNames;
  }
  /**
   * @return string[]
   */
  public function getInventorySecurityGroupNames()
  {
    return $this->inventorySecurityGroupNames;
  }
  /**
   * @param Tag[]
   */
  public function setInventoryTagList($inventoryTagList)
  {
    $this->inventoryTagList = $inventoryTagList;
  }
  /**
   * @return Tag[]
   */
  public function getInventoryTagList()
  {
    return $this->inventoryTagList;
  }
  /**
   * @param string[]
   */
  public function setMigrationResourcesUserTags($migrationResourcesUserTags)
  {
    $this->migrationResourcesUserTags = $migrationResourcesUserTags;
  }
  /**
   * @return string[]
   */
  public function getMigrationResourcesUserTags()
  {
    return $this->migrationResourcesUserTags;
  }
  /**
   * @param string
   */
  public function setPublicIp($publicIp)
  {
    $this->publicIp = $publicIp;
  }
  /**
   * @return string
   */
  public function getPublicIp()
  {
    return $this->publicIp;
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
class_alias(AwsSourceDetails::class, 'Google_Service_VMMigrationService_AwsSourceDetails');
