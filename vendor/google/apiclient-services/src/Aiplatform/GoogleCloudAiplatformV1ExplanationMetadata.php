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

class GoogleCloudAiplatformV1ExplanationMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $featureAttributionsSchemaUri;
  protected $inputsType = GoogleCloudAiplatformV1ExplanationMetadataInputMetadata::class;
  protected $inputsDataType = 'map';
  /**
   * @var string
   */
  public $latentSpaceSource;
  protected $outputsType = GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata::class;
  protected $outputsDataType = 'map';

  /**
   * @param string
   */
  public function setFeatureAttributionsSchemaUri($featureAttributionsSchemaUri)
  {
    $this->featureAttributionsSchemaUri = $featureAttributionsSchemaUri;
  }
  /**
   * @return string
   */
  public function getFeatureAttributionsSchemaUri()
  {
    return $this->featureAttributionsSchemaUri;
  }
  /**
   * @param GoogleCloudAiplatformV1ExplanationMetadataInputMetadata[]
   */
  public function setInputs($inputs)
  {
    $this->inputs = $inputs;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationMetadataInputMetadata[]
   */
  public function getInputs()
  {
    return $this->inputs;
  }
  /**
   * @param string
   */
  public function setLatentSpaceSource($latentSpaceSource)
  {
    $this->latentSpaceSource = $latentSpaceSource;
  }
  /**
   * @return string
   */
  public function getLatentSpaceSource()
  {
    return $this->latentSpaceSource;
  }
  /**
   * @param GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata[]
   */
  public function setOutputs($outputs)
  {
    $this->outputs = $outputs;
  }
  /**
   * @return GoogleCloudAiplatformV1ExplanationMetadataOutputMetadata[]
   */
  public function getOutputs()
  {
    return $this->outputs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExplanationMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExplanationMetadata');
