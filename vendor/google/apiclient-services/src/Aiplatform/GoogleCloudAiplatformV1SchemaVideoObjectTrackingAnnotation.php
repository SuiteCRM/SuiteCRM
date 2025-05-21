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

class GoogleCloudAiplatformV1SchemaVideoObjectTrackingAnnotation extends \Google\Model
{
  /**
   * @var string
   */
  public $annotationSpecId;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $instanceId;
  /**
   * @var string
   */
  public $timeOffset;
  public $xMax;
  public $xMin;
  public $yMax;
  public $yMin;

  /**
   * @param string
   */
  public function setAnnotationSpecId($annotationSpecId)
  {
    $this->annotationSpecId = $annotationSpecId;
  }
  /**
   * @return string
   */
  public function getAnnotationSpecId()
  {
    return $this->annotationSpecId;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setInstanceId($instanceId)
  {
    $this->instanceId = $instanceId;
  }
  /**
   * @return string
   */
  public function getInstanceId()
  {
    return $this->instanceId;
  }
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
  public function setXMax($xMax)
  {
    $this->xMax = $xMax;
  }
  public function getXMax()
  {
    return $this->xMax;
  }
  public function setXMin($xMin)
  {
    $this->xMin = $xMin;
  }
  public function getXMin()
  {
    return $this->xMin;
  }
  public function setYMax($yMax)
  {
    $this->yMax = $yMax;
  }
  public function getYMax()
  {
    return $this->yMax;
  }
  public function setYMin($yMin)
  {
    $this->yMin = $yMin;
  }
  public function getYMin()
  {
    return $this->yMin;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaVideoObjectTrackingAnnotation::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaVideoObjectTrackingAnnotation');
