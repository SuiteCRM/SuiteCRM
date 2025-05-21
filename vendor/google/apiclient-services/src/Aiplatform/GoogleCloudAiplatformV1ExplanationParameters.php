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

class GoogleCloudAiplatformV1ExplanationParameters extends \Google\Collection
{
  protected $collection_key = 'outputIndices';
  protected $examplesType = GoogleCloudAiplatformV1Examples::class;
  protected $examplesDataType = '';
  protected $integratedGradientsAttributionType = GoogleCloudAiplatformV1IntegratedGradientsAttribution::class;
  protected $integratedGradientsAttributionDataType = '';
  /**
   * @var array[]
   */
  public $outputIndices;
  protected $sampledShapleyAttributionType = GoogleCloudAiplatformV1SampledShapleyAttribution::class;
  protected $sampledShapleyAttributionDataType = '';
  /**
   * @var int
   */
  public $topK;
  protected $xraiAttributionType = GoogleCloudAiplatformV1XraiAttribution::class;
  protected $xraiAttributionDataType = '';

  /**
   * @param GoogleCloudAiplatformV1Examples
   */
  public function setExamples(GoogleCloudAiplatformV1Examples $examples)
  {
    $this->examples = $examples;
  }
  /**
   * @return GoogleCloudAiplatformV1Examples
   */
  public function getExamples()
  {
    return $this->examples;
  }
  /**
   * @param GoogleCloudAiplatformV1IntegratedGradientsAttribution
   */
  public function setIntegratedGradientsAttribution(GoogleCloudAiplatformV1IntegratedGradientsAttribution $integratedGradientsAttribution)
  {
    $this->integratedGradientsAttribution = $integratedGradientsAttribution;
  }
  /**
   * @return GoogleCloudAiplatformV1IntegratedGradientsAttribution
   */
  public function getIntegratedGradientsAttribution()
  {
    return $this->integratedGradientsAttribution;
  }
  /**
   * @param array[]
   */
  public function setOutputIndices($outputIndices)
  {
    $this->outputIndices = $outputIndices;
  }
  /**
   * @return array[]
   */
  public function getOutputIndices()
  {
    return $this->outputIndices;
  }
  /**
   * @param GoogleCloudAiplatformV1SampledShapleyAttribution
   */
  public function setSampledShapleyAttribution(GoogleCloudAiplatformV1SampledShapleyAttribution $sampledShapleyAttribution)
  {
    $this->sampledShapleyAttribution = $sampledShapleyAttribution;
  }
  /**
   * @return GoogleCloudAiplatformV1SampledShapleyAttribution
   */
  public function getSampledShapleyAttribution()
  {
    return $this->sampledShapleyAttribution;
  }
  /**
   * @param int
   */
  public function setTopK($topK)
  {
    $this->topK = $topK;
  }
  /**
   * @return int
   */
  public function getTopK()
  {
    return $this->topK;
  }
  /**
   * @param GoogleCloudAiplatformV1XraiAttribution
   */
  public function setXraiAttribution(GoogleCloudAiplatformV1XraiAttribution $xraiAttribution)
  {
    $this->xraiAttribution = $xraiAttribution;
  }
  /**
   * @return GoogleCloudAiplatformV1XraiAttribution
   */
  public function getXraiAttribution()
  {
    return $this->xraiAttribution;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExplanationParameters::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExplanationParameters');
