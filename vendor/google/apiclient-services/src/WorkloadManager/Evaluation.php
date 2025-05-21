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

namespace Google\Service\WorkloadManager;

class Evaluation extends \Google\Collection
{
  protected $collection_key = 'ruleVersions';
  protected $bigQueryDestinationType = BigQueryDestination::class;
  protected $bigQueryDestinationDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $customRulesBucket;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $evaluationType;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $resourceFilterType = ResourceFilter::class;
  protected $resourceFilterDataType = '';
  protected $resourceStatusType = ResourceStatus::class;
  protected $resourceStatusDataType = '';
  /**
   * @var string[]
   */
  public $ruleNames;
  /**
   * @var string[]
   */
  public $ruleVersions;
  /**
   * @var string
   */
  public $schedule;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param BigQueryDestination
   */
  public function setBigQueryDestination(BigQueryDestination $bigQueryDestination)
  {
    $this->bigQueryDestination = $bigQueryDestination;
  }
  /**
   * @return BigQueryDestination
   */
  public function getBigQueryDestination()
  {
    return $this->bigQueryDestination;
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
  public function setCustomRulesBucket($customRulesBucket)
  {
    $this->customRulesBucket = $customRulesBucket;
  }
  /**
   * @return string
   */
  public function getCustomRulesBucket()
  {
    return $this->customRulesBucket;
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
  public function setEvaluationType($evaluationType)
  {
    $this->evaluationType = $evaluationType;
  }
  /**
   * @return string
   */
  public function getEvaluationType()
  {
    return $this->evaluationType;
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
   * @param ResourceFilter
   */
  public function setResourceFilter(ResourceFilter $resourceFilter)
  {
    $this->resourceFilter = $resourceFilter;
  }
  /**
   * @return ResourceFilter
   */
  public function getResourceFilter()
  {
    return $this->resourceFilter;
  }
  /**
   * @param ResourceStatus
   */
  public function setResourceStatus(ResourceStatus $resourceStatus)
  {
    $this->resourceStatus = $resourceStatus;
  }
  /**
   * @return ResourceStatus
   */
  public function getResourceStatus()
  {
    return $this->resourceStatus;
  }
  /**
   * @param string[]
   */
  public function setRuleNames($ruleNames)
  {
    $this->ruleNames = $ruleNames;
  }
  /**
   * @return string[]
   */
  public function getRuleNames()
  {
    return $this->ruleNames;
  }
  /**
   * @param string[]
   */
  public function setRuleVersions($ruleVersions)
  {
    $this->ruleVersions = $ruleVersions;
  }
  /**
   * @return string[]
   */
  public function getRuleVersions()
  {
    return $this->ruleVersions;
  }
  /**
   * @param string
   */
  public function setSchedule($schedule)
  {
    $this->schedule = $schedule;
  }
  /**
   * @return string
   */
  public function getSchedule()
  {
    return $this->schedule;
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
class_alias(Evaluation::class, 'Google_Service_WorkloadManager_Evaluation');
