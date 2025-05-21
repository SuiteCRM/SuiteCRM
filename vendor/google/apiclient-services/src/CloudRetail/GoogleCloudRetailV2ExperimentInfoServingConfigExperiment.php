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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2ExperimentInfoServingConfigExperiment extends \Google\Model
{
  /**
   * @var string
   */
  public $experimentServingConfig;
  /**
   * @var string
   */
  public $originalServingConfig;

  /**
   * @param string
   */
  public function setExperimentServingConfig($experimentServingConfig)
  {
    $this->experimentServingConfig = $experimentServingConfig;
  }
  /**
   * @return string
   */
  public function getExperimentServingConfig()
  {
    return $this->experimentServingConfig;
  }
  /**
   * @param string
   */
  public function setOriginalServingConfig($originalServingConfig)
  {
    $this->originalServingConfig = $originalServingConfig;
  }
  /**
   * @return string
   */
  public function getOriginalServingConfig()
  {
    return $this->originalServingConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2ExperimentInfoServingConfigExperiment::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2ExperimentInfoServingConfigExperiment');
