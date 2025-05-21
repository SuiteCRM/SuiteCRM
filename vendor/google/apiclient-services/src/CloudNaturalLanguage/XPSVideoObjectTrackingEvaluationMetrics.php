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

class XPSVideoObjectTrackingEvaluationMetrics extends \Google\Collection
{
  protected $collection_key = 'trackMetricsEntries';
  /**
   * @var float
   */
  public $boundingBoxMeanAveragePrecision;
  protected $boundingBoxMetricsEntriesType = XPSBoundingBoxMetricsEntry::class;
  protected $boundingBoxMetricsEntriesDataType = 'array';
  /**
   * @var int
   */
  public $evaluatedBoundingboxCount;
  /**
   * @var int
   */
  public $evaluatedFrameCount;
  /**
   * @var int
   */
  public $evaluatedTrackCount;
  /**
   * @var float
   */
  public $trackMeanAveragePrecision;
  /**
   * @var float
   */
  public $trackMeanBoundingBoxIou;
  /**
   * @var float
   */
  public $trackMeanMismatchRate;
  protected $trackMetricsEntriesType = XPSTrackMetricsEntry::class;
  protected $trackMetricsEntriesDataType = 'array';

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
  public function setEvaluatedBoundingboxCount($evaluatedBoundingboxCount)
  {
    $this->evaluatedBoundingboxCount = $evaluatedBoundingboxCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedBoundingboxCount()
  {
    return $this->evaluatedBoundingboxCount;
  }
  /**
   * @param int
   */
  public function setEvaluatedFrameCount($evaluatedFrameCount)
  {
    $this->evaluatedFrameCount = $evaluatedFrameCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedFrameCount()
  {
    return $this->evaluatedFrameCount;
  }
  /**
   * @param int
   */
  public function setEvaluatedTrackCount($evaluatedTrackCount)
  {
    $this->evaluatedTrackCount = $evaluatedTrackCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedTrackCount()
  {
    return $this->evaluatedTrackCount;
  }
  /**
   * @param float
   */
  public function setTrackMeanAveragePrecision($trackMeanAveragePrecision)
  {
    $this->trackMeanAveragePrecision = $trackMeanAveragePrecision;
  }
  /**
   * @return float
   */
  public function getTrackMeanAveragePrecision()
  {
    return $this->trackMeanAveragePrecision;
  }
  /**
   * @param float
   */
  public function setTrackMeanBoundingBoxIou($trackMeanBoundingBoxIou)
  {
    $this->trackMeanBoundingBoxIou = $trackMeanBoundingBoxIou;
  }
  /**
   * @return float
   */
  public function getTrackMeanBoundingBoxIou()
  {
    return $this->trackMeanBoundingBoxIou;
  }
  /**
   * @param float
   */
  public function setTrackMeanMismatchRate($trackMeanMismatchRate)
  {
    $this->trackMeanMismatchRate = $trackMeanMismatchRate;
  }
  /**
   * @return float
   */
  public function getTrackMeanMismatchRate()
  {
    return $this->trackMeanMismatchRate;
  }
  /**
   * @param XPSTrackMetricsEntry[]
   */
  public function setTrackMetricsEntries($trackMetricsEntries)
  {
    $this->trackMetricsEntries = $trackMetricsEntries;
  }
  /**
   * @return XPSTrackMetricsEntry[]
   */
  public function getTrackMetricsEntries()
  {
    return $this->trackMetricsEntries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSVideoObjectTrackingEvaluationMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSVideoObjectTrackingEvaluationMetrics');
