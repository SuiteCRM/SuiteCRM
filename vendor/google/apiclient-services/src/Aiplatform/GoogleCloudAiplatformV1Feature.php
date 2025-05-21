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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1Feature extends \Google\Collection
{
  protected $collection_key = 'monitoringStatsAnomalies';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $disableMonitoring;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  protected $monitoringStatsAnomaliesType = GoogleCloudAiplatformV1FeatureMonitoringStatsAnomaly::class;
  protected $monitoringStatsAnomaliesDataType = 'array';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $pointOfContact;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $valueType;
  /**
   * @var string
   */
  public $versionColumnName;

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
   * @param bool
   */
  public function setDisableMonitoring($disableMonitoring)
  {
    $this->disableMonitoring = $disableMonitoring;
  }
  /**
   * @return bool
   */
  public function getDisableMonitoring()
  {
    return $this->disableMonitoring;
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
   * @param GoogleCloudAiplatformV1FeatureMonitoringStatsAnomaly[]
   */
  public function setMonitoringStatsAnomalies($monitoringStatsAnomalies)
  {
    $this->monitoringStatsAnomalies = $monitoringStatsAnomalies;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureMonitoringStatsAnomaly[]
   */
  public function getMonitoringStatsAnomalies()
  {
    return $this->monitoringStatsAnomalies;
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
  public function setPointOfContact($pointOfContact)
  {
    $this->pointOfContact = $pointOfContact;
  }
  /**
   * @return string
   */
  public function getPointOfContact()
  {
    return $this->pointOfContact;
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
  /**
   * @param string
   */
  public function setValueType($valueType)
  {
    $this->valueType = $valueType;
  }
  /**
   * @return string
   */
  public function getValueType()
  {
    return $this->valueType;
  }
  /**
   * @param string
   */
  public function setVersionColumnName($versionColumnName)
  {
    $this->versionColumnName = $versionColumnName;
  }
  /**
   * @return string
   */
  public function getVersionColumnName()
  {
    return $this->versionColumnName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Feature::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Feature');
