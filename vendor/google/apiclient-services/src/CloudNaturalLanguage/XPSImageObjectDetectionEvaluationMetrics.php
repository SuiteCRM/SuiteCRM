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

namespace Google\Service\CloudNaturalLanguage;

class XPSImageObjectDetectionEvaluationMetrics extends \Google\Collection
{
  protected $collection_key = 'boundingBoxMetricsEntries';
  /**
   * @var float
   */
  public $boundingBoxMeanAveragePrecision;
  protected $boundingBoxMetricsEntriesType = XPSBoundingBoxMetricsEntry::class;
  protected $boundingBoxMetricsEntriesDataType = 'array';
  /**
   * @var int
   */
  public $evaluatedBoundingBoxCount;

  /**
   * @param float
   */
  public function setBoundingBoxMeanAveragePrecision($boundingBoxMeanAveragePrecision)
  {
    $this->boundingBoxMeanAveragePrecision = $boundingBoxMeanAveragePrecision;
  }
  /**
   * @return float
   */
  public function getBoundingBoxMeanAveragePrecision()
  {
    return $this->boundingBoxMeanAveragePrecision;
  }
  /**
   * @param XPSBoundingBoxMetricsEntry[]
   */
  public function setBoundingBoxMetricsEntries($boundingBoxMetricsEntries)
  {
    $this->boundingBoxMetricsEntries = $boundingBoxMetricsEntries;
  }
  /**
   * @return XPSBoundingBoxMetricsEntry[]
   */
  public function getBoundingBoxMetricsEntries()
  {
    return $this->boundingBoxMetricsEntries;
  }
  /**
   * @param int
   */
  public function setEvaluatedBoundingBoxCount($evaluatedBoundingBoxCount)
  {
    $this->evaluatedBoundingBoxCount = $evaluatedBoundingBoxCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedBoundingBoxCount()
  {
    return $this->evaluatedBoundingBoxCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSImageObjectDetectionEvaluationMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSImageObjectDetectionEvaluationMetrics');
