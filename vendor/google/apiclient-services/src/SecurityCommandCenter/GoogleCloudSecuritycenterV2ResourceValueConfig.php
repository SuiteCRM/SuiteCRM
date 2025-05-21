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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2ResourceValueConfig extends \Google\Collection
{
  protected $collection_key = 'tagValues';
  /**
   * @var string
   */
  public $cloudProvider;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $resourceLabelsSelector;
  /**
   * @var string
   */
  public $resourceType;
  /**
   * @var string
   */
  public $resourceValue;
  /**
   * @var string
   */
  public $scope;
  protected $sensitiveDataProtectionMappingType = GoogleCloudSecuritycenterV2SensitiveDataProtectionMapping::class;
  protected $sensitiveDataProtectionMappingDataType = '';
  /**
   * @var string[]
   */
  public $tagValues;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCloudProvider($cloudProvider)
  {
    $this->cloudProvider = $cloudProvider;
  }
  /**
   * @return string
   */
  public function getCloudProvider()
  {
    return $this->cloudProvider;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
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
   * @param string[]
   */
  public function setResourceLabelsSelector($resourceLabelsSelector)
  {
    $this->resourceLabelsSelector = $resourceLabelsSelector;
  }
  /**
   * @return string[]
   */
  public function getResourceLabelsSelector()
  {
    return $this->resourceLabelsSelector;
  }
  /**
   * @param string
   */
  public function setResourceType($resourceType)
  {
    $this->resourceType = $resourceType;
  }
  /**
   * @return string
   */
  public function getResourceType()
  {
    return $this->resourceType;
  }
  /**
   * @param string
   */
  public function setResourceValue($resourceValue)
  {
    $this->resourceValue = $resourceValue;
  }
  /**
   * @return string
   */
  public function getResourceValue()
  {
    return $this->resourceValue;
  }
  /**
   * @param string
   */
  public function setScope($scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return string
   */
  public function getScope()
  {
    return $this->scope;
  }
  /**
   * @param GoogleCloudSecuritycenterV2SensitiveDataProtectionMapping
   */
  public function setSensitiveDataProtectionMapping(GoogleCloudSecuritycenterV2SensitiveDataProtectionMapping $sensitiveDataProtectionMapping)
  {
    $this->sensitiveDataProtectionMapping = $sensitiveDataProtectionMapping;
  }
  /**
   * @return GoogleCloudSecuritycenterV2SensitiveDataProtectionMapping
   */
  public function getSensitiveDataProtectionMapping()
  {
    return $this->sensitiveDataProtectionMapping;
  }
  /**
   * @param string[]
   */
  public function setTagValues($tagValues)
  {
    $this->tagValues = $tagValues;
  }
  /**
   * @return string[]
   */
  public function getTagValues()
  {
    return $this->tagValues;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2ResourceValueConfig::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2ResourceValueConfig');
