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

class GoogleCloudDataplexV1Entry extends \Google\Model
{
  protected $aspectsType = GoogleCloudDataplexV1Aspect::class;
  protected $aspectsDataType = 'map';
  /**
   * @var string
   */
  public $createTime;
  protected $entrySourceType = GoogleCloudDataplexV1EntrySource::class;
  protected $entrySourceDataType = '';
  /**
   * @var string
   */
  public $entryType;
  /**
   * @var string
   */
  public $fullyQualifiedName;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $parentEntry;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDataplexV1Aspect[]
   */
  public function setAspects($aspects)
  {
    $this->aspects = $aspects;
  }
  /**
   * @return GoogleCloudDataplexV1Aspect[]
   */
  public function getAspects()
  {
    return $this->aspects;
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
   * @param GoogleCloudDataplexV1EntrySource
   */
  public function setEntrySource(GoogleCloudDataplexV1EntrySource $entrySource)
  {
    $this->entrySource = $entrySource;
  }
  /**
   * @return GoogleCloudDataplexV1EntrySource
   */
  public function getEntrySource()
  {
    return $this->entrySource;
  }
  /**
   * @param string
   */
  public function setEntryType($entryType)
  {
    $this->entryType = $entryType;
  }
  /**
   * @return string
   */
  public function getEntryType()
  {
    return $this->entryType;
  }
  /**
   * @param string
   */
  public function setFullyQualifiedName($fullyQualifiedName)
  {
    $this->fullyQualifiedName = $fullyQualifiedName;
  }
  /**
   * @return string
   */
  public function getFullyQualifiedName()
  {
    return $this->fullyQualifiedName;
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
  public function setParentEntry($parentEntry)
  {
    $this->parentEntry = $parentEntry;
  }
  /**
   * @return string
   */
  public function getParentEntry()
  {
    return $this->parentEntry;
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
class_alias(GoogleCloudDataplexV1Entry::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1Entry');
