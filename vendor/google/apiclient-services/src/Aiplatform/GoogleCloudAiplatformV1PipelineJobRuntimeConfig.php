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

class GoogleCloudAiplatformV1PipelineJobRuntimeConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $failurePolicy;
  /**
   * @var string
   */
  public $gcsOutputDirectory;
  protected $inputArtifactsType = GoogleCloudAiplatformV1PipelineJobRuntimeConfigInputArtifact::class;
  protected $inputArtifactsDataType = 'map';
  /**
   * @var array[]
   */
  public $parameterValues;
  protected $parametersType = GoogleCloudAiplatformV1Value::class;
  protected $parametersDataType = 'map';

  /**
   * @param string
   */
  public function setFailurePolicy($failurePolicy)
  {
    $this->failurePolicy = $failurePolicy;
  }
  /**
   * @return string
   */
  public function getFailurePolicy()
  {
    return $this->failurePolicy;
  }
  /**
   * @param string
   */
  public function setGcsOutputDirectory($gcsOutputDirectory)
  {
    $this->gcsOutputDirectory = $gcsOutputDirectory;
  }
  /**
   * @return string
   */
  public function getGcsOutputDirectory()
  {
    return $this->gcsOutputDirectory;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineJobRuntimeConfigInputArtifact[]
   */
  public function setInputArtifacts($inputArtifacts)
  {
    $this->inputArtifacts = $inputArtifacts;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineJobRuntimeConfigInputArtifact[]
   */
  public function getInputArtifacts()
  {
    return $this->inputArtifacts;
  }
  /**
   * @param array[]
   */
  public function setParameterValues($parameterValues)
  {
    $this->parameterValues = $parameterValues;
  }
  /**
   * @return array[]
   */
  public function getParameterValues()
  {
    return $this->parameterValues;
  }
  /**
   * @param GoogleCloudAiplatformV1Value[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return GoogleCloudAiplatformV1Value[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PipelineJobRuntimeConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PipelineJobRuntimeConfig');
