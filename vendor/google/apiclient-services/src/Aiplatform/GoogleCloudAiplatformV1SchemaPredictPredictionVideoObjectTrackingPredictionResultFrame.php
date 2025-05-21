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

class GoogleCloudAiplatformV1SchemaPredictPredictionVideoObjectTrackingPredictionResultFrame extends \Google\Model
{
  /**
   * @var string
   */
  public $timeOffset;
  /**
   * @var float
   */
  public $xMax;
  /**
   * @var float
   */
  public $xMin;
  /**
   * @var float
   */
  public $yMax;
  /**
   * @var float
   */
  public $yMin;

  /**
   * @param string
   */
  public function setTimeOffset($timeOffset)
  {
    $this->timeOffset = $timeOffset;
  }
  /**
   * @return string
   */
  public function getTimeOffset()
  {
    return $this->timeOffset;
  }
  /**
   * @param float
   */
  public function setXMax($xMax)
  {
    $this->xMax = $xMax;
  }
  /**
   * @return float
   */
  public function getXMax()
  {
    return $this->xMax;
  }
  /**
   * @param float
   */
  public function setXMin($xMin)
  {
    $this->xMin = $xMin;
  }
  /**
   * @return float
   */
  public function getXMin()
  {
    return $this->xMin;
  }
  /**
   * @param float
   */
  public function setYMax($yMax)
  {
    $this->yMax = $yMax;
  }
  /**
   * @return float
   */
  public function getYMax()
  {
    return $this->yMax;
  }
  /**
   * @param float
   */
  public function setYMin($yMin)
  {
    $this->yMin = $yMin;
  }
  /**
   * @return float
   */
  public function getYMin()
  {
    return $this->yMin;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictPredictionVideoObjectTrackingPredictionResultFrame::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictPredictionVideoObjectTrackingPredictionResultFrame');
