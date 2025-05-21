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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1betaEvaluation extends \Google\Collection
{
  protected $collection_key = 'errorSamples';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  protected $errorSamplesType = GoogleRpcStatus::class;
  protected $errorSamplesDataType = 'array';
  protected $evaluationSpecType = GoogleCloudDiscoveryengineV1betaEvaluationEvaluationSpec::class;
  protected $evaluationSpecDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $qualityMetricsType = GoogleCloudDiscoveryengineV1betaQualityMetrics::class;
  protected $qualityMetricsDataType = '';
  /**
   * @var string
   */
  public $state;

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
   * @param GoogleRpcStatus
   */
  public function setError(GoogleRpcStatus $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param GoogleRpcStatus[]
   */
  public function setErrorSamples($errorSamples)
  {
    $this->errorSamples = $errorSamples;
  }
  /**
   * @return GoogleRpcStatus[]
   */
  public function getErrorSamples()
  {
    return $this->errorSamples;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaEvaluationEvaluationSpec
   */
  public function setEvaluationSpec(GoogleCloudDiscoveryengineV1betaEvaluationEvaluationSpec $evaluationSpec)
  {
    $this->evaluationSpec = $evaluationSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEvaluationEvaluationSpec
   */
  public function getEvaluationSpec()
  {
    return $this->evaluationSpec;
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
   * @param GoogleCloudDiscoveryengineV1betaQualityMetrics
   */
  public function setQualityMetrics(GoogleCloudDiscoveryengineV1betaQualityMetrics $qualityMetrics)
  {
    $this->qualityMetrics = $qualityMetrics;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaQualityMetrics
   */
  public function getQualityMetrics()
  {
    return $this->qualityMetrics;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaEvaluation::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaEvaluation');
