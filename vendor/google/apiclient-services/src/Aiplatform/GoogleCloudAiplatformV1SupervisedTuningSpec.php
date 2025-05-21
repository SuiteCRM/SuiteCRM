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

class GoogleCloudAiplatformV1SupervisedTuningSpec extends \Google\Model
{
  protected $hyperParametersType = GoogleCloudAiplatformV1SupervisedHyperParameters::class;
  protected $hyperParametersDataType = '';
  /**
   * @var string
   */
  public $trainingDatasetUri;
  /**
   * @var string
   */
  public $validationDatasetUri;

  /**
   * @param GoogleCloudAiplatformV1SupervisedHyperParameters
   */
  public function setHyperParameters(GoogleCloudAiplatformV1SupervisedHyperParameters $hyperParameters)
  {
    $this->hyperParameters = $hyperParameters;
  }
  /**
   * @return GoogleCloudAiplatformV1SupervisedHyperParameters
   */
  public function getHyperParameters()
  {
    return $this->hyperParameters;
  }
  /**
   * @param string
   */
  public function setTrainingDatasetUri($trainingDatasetUri)
  {
    $this->trainingDatasetUri = $trainingDatasetUri;
  }
  /**
   * @return string
   */
  public function getTrainingDatasetUri()
  {
    return $this->trainingDatasetUri;
  }
  /**
   * @param string
   */
  public function setValidationDatasetUri($validationDatasetUri)
  {
    $this->validationDatasetUri = $validationDatasetUri;
  }
  /**
   * @return string
   */
  public function getValidationDatasetUri()
  {
    return $this->validationDatasetUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SupervisedTuningSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SupervisedTuningSpec');
