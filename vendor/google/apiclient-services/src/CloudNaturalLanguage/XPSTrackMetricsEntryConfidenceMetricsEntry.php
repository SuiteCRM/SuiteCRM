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

class XPSTrackMetricsEntryConfidenceMetricsEntry extends \Google\Model
{
  /**
   * @var float
   */
  public $boundingBoxIou;
  /**
   * @var float
   */
  public $confidenceThreshold;
  /**
   * @var float
   */
  public $mismatchRate;
  /**
   * @var float
   */
  public $trackingPrecision;
  /**
   * @var float
   */
  public $trackingRecall;

  /**
   * @param float
   */
  public function setBoundingBoxIou($boundingBoxIou)
  {
    $this->boundingBoxIou = $boundingBoxIou;
  }
  /**
   * @return float
   */
  public function getBoundingBoxIou()
  {
    return $this->boundingBoxIou;
  }
  /**
   * @param float
   */
  public function setConfidenceThreshold($confidenceThreshold)
  {
    $this->confidenceThreshold = $confidenceThreshold;
  }
  /**
   * @return float
   */
  public function getConfidenceThreshold()
  {
    return $this->confidenceThreshold;
  }
  /**
   * @param float
   */
  public function setMismatchRate($mismatchRate)
  {
    $this->mismatchRate = $mismatchRate;
  }
  /**
   * @return float
   */
  public function getMismatchRate()
  {
    return $this->mismatchRate;
  }
  /**
   * @param float
   */
  public function setTrackingPrecision($trackingPrecision)
  {
    $this->trackingPrecision = $trackingPrecision;
  }
  /**
   * @return float
   */
  public function getTrackingPrecision()
  {
    return $this->trackingPrecision;
  }
  /**
   * @param float
   */
  public function setTrackingRecall($trackingRecall)
  {
    $this->trackingRecall = $trackingRecall;
  }
  /**
   * @return float
   */
  public function getTrackingRecall()
  {
    return $this->trackingRecall;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTrackMetricsEntryConfidenceMetricsEntry::class, 'Google_Service_CloudNaturalLanguage_XPSTrackMetricsEntryConfidenceMetricsEntry');
