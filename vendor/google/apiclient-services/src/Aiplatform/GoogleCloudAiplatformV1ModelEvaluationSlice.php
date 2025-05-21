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

class GoogleCloudAiplatformV1ModelEvaluationSlice extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var array
   */
  public $metrics;
  /**
   * @var string
   */
  public $metricsSchemaUri;
  protected $modelExplanationType = GoogleCloudAiplatformV1ModelExplanation::class;
  protected $modelExplanationDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $sliceType = GoogleCloudAiplatformV1ModelEvaluationSliceSlice::class;
  protected $sliceDataType = '';

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
   * @param array
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return array
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param string
   */
  public function setMetricsSchemaUri($metricsSchemaUri)
  {
    $this->metricsSchemaUri = $metricsSchemaUri;
  }
  /**
   * @return string
   */
  public function getMetricsSchemaUri()
  {
    return $this->metricsSchemaUri;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelExplanation
   */
  public function setModelExplanation(GoogleCloudAiplatformV1ModelExplanation $modelExplanation)
  {
    $this->modelExplanation = $modelExplanation;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelExplanation
   */
  public function getModelExplanation()
  {
    return $this->modelExplanation;
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
   * @param GoogleCloudAiplatformV1ModelEvaluationSliceSlice
   */
  public function setSlice(GoogleCloudAiplatformV1ModelEvaluationSliceSlice $slice)
  {
    $this->slice = $slice;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelEvaluationSliceSlice
   */
  public function getSlice()
  {
    return $this->slice;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelEvaluationSlice::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelEvaluationSlice');
