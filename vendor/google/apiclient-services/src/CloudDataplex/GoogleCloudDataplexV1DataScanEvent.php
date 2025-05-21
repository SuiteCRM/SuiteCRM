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

class GoogleCloudDataplexV1DataScanEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $dataProfileType = GoogleCloudDataplexV1DataScanEventDataProfileResult::class;
  protected $dataProfileDataType = '';
  protected $dataProfileConfigsType = GoogleCloudDataplexV1DataScanEventDataProfileAppliedConfigs::class;
  protected $dataProfileConfigsDataType = '';
  protected $dataQualityType = GoogleCloudDataplexV1DataScanEventDataQualityResult::class;
  protected $dataQualityDataType = '';
  protected $dataQualityConfigsType = GoogleCloudDataplexV1DataScanEventDataQualityAppliedConfigs::class;
  protected $dataQualityConfigsDataType = '';
  /**
   * @var string
   */
  public $dataSource;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $jobId;
  /**
   * @var string
   */
  public $message;
  protected $postScanActionsResultType = GoogleCloudDataplexV1DataScanEventPostScanActionsResult::class;
  protected $postScanActionsResultDataType = '';
  /**
   * @var string
   */
  public $scope;
  /**
   * @var string
   */
  public $specVersion;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $trigger;
  /**
   * @var string
   */
  public $type;

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
   * @param GoogleCloudDataplexV1DataScanEventDataProfileResult
   */
  public function setDataProfile(GoogleCloudDataplexV1DataScanEventDataProfileResult $dataProfile)
  {
    $this->dataProfile = $dataProfile;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanEventDataProfileResult
   */
  public function getDataProfile()
  {
    return $this->dataProfile;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanEventDataProfileAppliedConfigs
   */
  public function setDataProfileConfigs(GoogleCloudDataplexV1DataScanEventDataProfileAppliedConfigs $dataProfileConfigs)
  {
    $this->dataProfileConfigs = $dataProfileConfigs;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanEventDataProfileAppliedConfigs
   */
  public function getDataProfileConfigs()
  {
    return $this->dataProfileConfigs;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanEventDataQualityResult
   */
  public function setDataQuality(GoogleCloudDataplexV1DataScanEventDataQualityResult $dataQuality)
  {
    $this->dataQuality = $dataQuality;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanEventDataQualityResult
   */
  public function getDataQuality()
  {
    return $this->dataQuality;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanEventDataQualityAppliedConfigs
   */
  public function setDataQualityConfigs(GoogleCloudDataplexV1DataScanEventDataQualityAppliedConfigs $dataQualityConfigs)
  {
    $this->dataQualityConfigs = $dataQualityConfigs;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanEventDataQualityAppliedConfigs
   */
  public function getDataQualityConfigs()
  {
    return $this->dataQualityConfigs;
  }
  /**
   * @param string
   */
  public function setDataSource($dataSource)
  {
    $this->dataSource = $dataSource;
  }
  /**
   * @return string
   */
  public function getDataSource()
  {
    return $this->dataSource;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param string
   */
  public function setJobId($jobId)
  {
    $this->jobId = $jobId;
  }
  /**
   * @return string
   */
  public function getJobId()
  {
    return $this->jobId;
  }
  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
  /**
   * @param GoogleCloudDataplexV1DataScanEventPostScanActionsResult
   */
  public function setPostScanActionsResult(GoogleCloudDataplexV1DataScanEventPostScanActionsResult $postScanActionsResult)
  {
    $this->postScanActionsResult = $postScanActionsResult;
  }
  /**
   * @return GoogleCloudDataplexV1DataScanEventPostScanActionsResult
   */
  public function getPostScanActionsResult()
  {
    return $this->postScanActionsResult;
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
   * @param string
   */
  public function setSpecVersion($specVersion)
  {
    $this->specVersion = $specVersion;
  }
  /**
   * @return string
   */
  public function getSpecVersion()
  {
    return $this->specVersion;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
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
  /**
   * @param string
   */
  public function setTrigger($trigger)
  {
    $this->trigger = $trigger;
  }
  /**
   * @return string
   */
  public function getTrigger()
  {
    return $this->trigger;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataScanEvent::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataScanEvent');
