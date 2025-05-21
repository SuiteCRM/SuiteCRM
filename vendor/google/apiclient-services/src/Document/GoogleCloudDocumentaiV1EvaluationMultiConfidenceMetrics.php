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

namespace Google\Service\Document;

class GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics extends \Google\Collection
{
  protected $collection_key = 'confidenceLevelMetricsExact';
  /**
   * @var float
   */
  public $auprc;
  /**
   * @var float
   */
  public $auprcExact;
  protected $confidenceLevelMetricsType = GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics::class;
  protected $confidenceLevelMetricsDataType = 'array';
  protected $confidenceLevelMetricsExactType = GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics::class;
  protected $confidenceLevelMetricsExactDataType = 'array';
  /**
   * @var float
   */
  public $estimatedCalibrationError;
  /**
   * @var float
   */
  public $estimatedCalibrationErrorExact;
  /**
   * @var string
   */
  public $metricsType;

  /**
   * @param float
   */
  public function setAuprc($auprc)
  {
    $this->auprc = $auprc;
  }
  /**
   * @return float
   */
  public function getAuprc()
  {
    return $this->auprc;
  }
  /**
   * @param float
   */
  public function setAuprcExact($auprcExact)
  {
    $this->auprcExact = $auprcExact;
  }
  /**
   * @return float
   */
  public function getAuprcExact()
  {
    return $this->auprcExact;
  }
  /**
   * @param GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics[]
   */
  public function setConfidenceLevelMetrics($confidenceLevelMetrics)
  {
    $this->confidenceLevelMetrics = $confidenceLevelMetrics;
  }
  /**
   * @return GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics[]
   */
  public function getConfidenceLevelMetrics()
  {
    return $this->confidenceLevelMetrics;
  }
  /**
   * @param GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics[]
   */
  public function setConfidenceLevelMetricsExact($confidenceLevelMetricsExact)
  {
    $this->confidenceLevelMetricsExact = $confidenceLevelMetricsExact;
  }
  /**
   * @return GoogleCloudDocumentaiV1EvaluationConfidenceLevelMetrics[]
   */
  public function getConfidenceLevelMetricsExact()
  {
    return $this->confidenceLevelMetricsExact;
  }
  /**
   * @param float
   */
  public function setEstimatedCalibrationError($estimatedCalibrationError)
  {
    $this->estimatedCalibrationError = $estimatedCalibrationError;
  }
  /**
   * @return float
   */
  public function getEstimatedCalibrationError()
  {
    return $this->estimatedCalibrationError;
  }
  /**
   * @param float
   */
  public function setEstimatedCalibrationErrorExact($estimatedCalibrationErrorExact)
  {
    $this->estimatedCalibrationErrorExact = $estimatedCalibrationErrorExact;
  }
  /**
   * @return float
   */
  public function getEstimatedCalibrationErrorExact()
  {
    return $this->estimatedCalibrationErrorExact;
  }
  /**
   * @param string
   */
  public function setMetricsType($metricsType)
  {
    $this->metricsType = $metricsType;
  }
  /**
   * @return string
   */
  public function getMetricsType()
  {
    return $this->metricsType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics::class, 'Google_Service_Document_GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics');
