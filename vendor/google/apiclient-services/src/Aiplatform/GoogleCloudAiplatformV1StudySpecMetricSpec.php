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

class GoogleCloudAiplatformV1StudySpecMetricSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $goal;
  /**
   * @var string
   */
  public $metricId;
  protected $safetyConfigType = GoogleCloudAiplatformV1StudySpecMetricSpecSafetyMetricConfig::class;
  protected $safetyConfigDataType = '';

  /**
   * @param string
   */
  public function setGoal($goal)
  {
    $this->goal = $goal;
  }
  /**
   * @return string
   */
  public function getGoal()
  {
    return $this->goal;
  }
  /**
   * @param string
   */
  public function setMetricId($metricId)
  {
    $this->metricId = $metricId;
  }
  /**
   * @return string
   */
  public function getMetricId()
  {
    return $this->metricId;
  }
  /**
   * @param GoogleCloudAiplatformV1StudySpecMetricSpecSafetyMetricConfig
   */
  public function setSafetyConfig(GoogleCloudAiplatformV1StudySpecMetricSpecSafetyMetricConfig $safetyConfig)
  {
    $this->safetyConfig = $safetyConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1StudySpecMetricSpecSafetyMetricConfig
   */
  public function getSafetyConfig()
  {
    return $this->safetyConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1StudySpecMetricSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1StudySpecMetricSpec');
