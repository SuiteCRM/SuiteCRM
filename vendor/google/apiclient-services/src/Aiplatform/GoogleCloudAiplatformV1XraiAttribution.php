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

class GoogleCloudAiplatformV1XraiAttribution extends \Google\Model
{
  protected $blurBaselineConfigType = GoogleCloudAiplatformV1BlurBaselineConfig::class;
  protected $blurBaselineConfigDataType = '';
  protected $smoothGradConfigType = GoogleCloudAiplatformV1SmoothGradConfig::class;
  protected $smoothGradConfigDataType = '';
  /**
   * @var int
   */
  public $stepCount;

  /**
   * @param GoogleCloudAiplatformV1BlurBaselineConfig
   */
  public function setBlurBaselineConfig(GoogleCloudAiplatformV1BlurBaselineConfig $blurBaselineConfig)
  {
    $this->blurBaselineConfig = $blurBaselineConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1BlurBaselineConfig
   */
  public function getBlurBaselineConfig()
  {
    return $this->blurBaselineConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1SmoothGradConfig
   */
  public function setSmoothGradConfig(GoogleCloudAiplatformV1SmoothGradConfig $smoothGradConfig)
  {
    $this->smoothGradConfig = $smoothGradConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1SmoothGradConfig
   */
  public function getSmoothGradConfig()
  {
    return $this->smoothGradConfig;
  }
  /**
   * @param int
   */
  public function setStepCount($stepCount)
  {
    $this->stepCount = $stepCount;
  }
  /**
   * @return int
   */
  public function getStepCount()
  {
    return $this->stepCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1XraiAttribution::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1XraiAttribution');
