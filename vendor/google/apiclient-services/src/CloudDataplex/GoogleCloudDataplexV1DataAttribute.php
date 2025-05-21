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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataAttribute extends \Google\Model
{
  /**
   * @var int
   */
  public $attributeCount;
  /**
   * @var string
   */
  public $createTime;
  protected $dataAccessSpecType = GoogleCloudDataplexV1DataAccessSpec::class;
  protected $dataAccessSpecDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $parentId;
  protected $resourceAccessSpecType = GoogleCloudDataplexV1ResourceAccessSpec::class;
  protected $resourceAccessSpecDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param int
   */
  public function setAttributeCount($attributeCount)
  {
    $this->attributeCount = $attributeCount;
  }
  /**
   * @return int
   */
  public function getAttributeCount()
  {
    return $this->attributeCount;
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
   * @param GoogleCloudDataplexV1DataAccessSpec
   */
  public function setDataAccessSpec(GoogleCloudDataplexV1DataAccessSpec $dataAccessSpec)
  {
    $this->dataAccessSpec = $dataAccessSpec;
  }
  /**
   * @return GoogleCloudDataplexV1DataAccessSpec
   */
  public function getDataAccessSpec()
  {
    return $this->dataAccessSpec;
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
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setParentId($parentId)
  {
    $this->parentId = $parentId;
  }
  /**
   * @return string
   */
  public function getParentId()
  {
    return $this->parentId;
  }
  /**
   * @param GoogleCloudDataplexV1ResourceAccessSpec
   */
  public function setResourceAccessSpec(GoogleCloudDataplexV1ResourceAccessSpec $resourceAccessSpec)
  {
    $this->resourceAccessSpec = $resourceAccessSpec;
  }
  /**
   * @return GoogleCloudDataplexV1ResourceAccessSpec
   */
  public function getResourceAccessSpec()
  {
    return $this->resourceAccessSpec;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
class_alias(GoogleCloudDataplexV1DataAttribute::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataAttribute');
