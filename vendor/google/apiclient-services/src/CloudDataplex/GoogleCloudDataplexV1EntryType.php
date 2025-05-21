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

class GoogleCloudDataplexV1EntryType extends \Google\Collection
{
  protected $collection_key = 'typeAliases';
  protected $authorizationType = GoogleCloudDataplexV1EntryTypeAuthorization::class;
  protected $authorizationDataType = '';
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
  public $platform;
  protected $requiredAspectsType = GoogleCloudDataplexV1EntryTypeAspectInfo::class;
  protected $requiredAspectsDataType = 'array';
  /**
   * @var string
   */
  public $system;
  /**
   * @var string[]
   */
  public $typeAliases;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDataplexV1EntryTypeAuthorization
   */
  public function setAuthorization(GoogleCloudDataplexV1EntryTypeAuthorization $authorization)
  {
    $this->authorization = $authorization;
  }
  /**
   * @return GoogleCloudDataplexV1EntryTypeAuthorization
   */
  public function getAuthorization()
  {
    return $this->authorization;
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
  public function setPlatform($platform)
  {
    $this->platform = $platform;
  }
  /**
   * @return string
   */
  public function getPlatform()
  {
    return $this->platform;
  }
  /**
   * @param GoogleCloudDataplexV1EntryTypeAspectInfo[]
   */
  public function setRequiredAspects($requiredAspects)
  {
    $this->requiredAspects = $requiredAspects;
  }
  /**
   * @return GoogleCloudDataplexV1EntryTypeAspectInfo[]
   */
  public function getRequiredAspects()
  {
    return $this->requiredAspects;
  }
  /**
   * @param string
   */
  public function setSystem($system)
  {
    $this->system = $system;
  }
  /**
   * @return string
   */
  public function getSystem()
  {
    return $this->system;
  }
  /**
   * @param string[]
   */
  public function setTypeAliases($typeAliases)
  {
    $this->typeAliases = $typeAliases;
  }
  /**
   * @return string[]
   */
  public function getTypeAliases()
  {
    return $this->typeAliases;
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
class_alias(GoogleCloudDataplexV1EntryType::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1EntryType');
