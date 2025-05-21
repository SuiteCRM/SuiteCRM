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

class GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetrics extends \Google\Collection
{
  protected $collection_key = 'confidenceMetrics';
  protected $confidenceMetricsType = GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetricsConfidenceMetrics::class;
  protected $confidenceMetricsDataType = 'array';
  /**
   * @var float
   */
  public $iouThreshold;
  /**
   * @var float
   */
  public $meanBoundingBoxIou;
  /**
   * @var float
   */
  public $meanMismatchRate;
  /**
   * @var float
   */
  public $meanTrackingAveragePrecision;

  /**
   * @param GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetricsConfidenceMetrics[]
   */
  public function setConfidenceMetrics($confidenceMetrics)
  {
    $this->confidenceMetrics = $confidenceMetrics;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetricsConfidenceMetrics[]
   */
  public function getConfidenceMetrics()
  {
    return $this->confidenceMetrics;
  }
  /**
   * @param float
   */
  public function setIouThreshold($iouThreshold)
  {
    $this->iouThreshold = $iouThreshold;
  }
  /**
   * @return float
   */
  public function getIouThreshold()
  {
    return $this->iouThreshold;
  }
  /**
   * @param float
   */
  public function setMeanBoundingBoxIou($meanBoundingBoxIou)
  {
    $this->meanBoundingBoxIou = $meanBoundingBoxIou;
  }
  /**
   * @return float
   */
  public function getMeanBoundingBoxIou()
  {
    return $this->meanBoundingBoxIou;
  }
  /**
   * @param float
   */
  public function setMeanMismatchRate($meanMismatchRate)
  {
    $this->meanMismatchRate = $meanMismatchRate;
  }
  /**
   * @return float
   */
  public function getMeanMismatchRate()
  {
    return $this->meanMismatchRate;
  }
  /**
   * @param float
   */
  public function setMeanTrackingAveragePrecision($meanTrackingAveragePrecision)
  {
    $this->meanTrackingAveragePrecision = $meanTrackingAveragePrecision;
  }
  /**
   * @return float
   */
  public function getMeanTrackingAveragePrecision()
  {
    return $this->meanTrackingAveragePrecision;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetrics::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaModelevaluationMetricsTrackMetrics');
