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

class GoogleCloudAiplatformV1RaySpec extends \Google\Model
{
  /**
   * @var string
   */
  public $headNodeResourcePoolId;
  /**
   * @var string
   */
  public $imageUri;
  protected $rayLogsSpecType = GoogleCloudAiplatformV1RayLogsSpec::class;
  protected $rayLogsSpecDataType = '';
  protected $rayMetricSpecType = GoogleCloudAiplatformV1RayMetricSpec::class;
  protected $rayMetricSpecDataType = '';
  /**
   * @var string[]
   */
  public $resourcePoolImages;

  /**
   * @param string
   */
  public function setHeadNodeResourcePoolId($headNodeResourcePoolId)
  {
    $this->headNodeResourcePoolId = $headNodeResourcePoolId;
  }
  /**
   * @return string
   */
  public function getHeadNodeResourcePoolId()
  {
    return $this->headNodeResourcePoolId;
  }
  /**
   * @param string
   */
  public function setImageUri($imageUri)
  {
    $this->imageUri = $imageUri;
  }
  /**
   * @return string
   */
  public function getImageUri()
  {
    return $this->imageUri;
  }
  /**
   * @param GoogleCloudAiplatformV1RayLogsSpec
   */
  public function setRayLogsSpec(GoogleCloudAiplatformV1RayLogsSpec $rayLogsSpec)
  {
    $this->rayLogsSpec = $rayLogsSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1RayLogsSpec
   */
  public function getRayLogsSpec()
  {
    return $this->rayLogsSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1RayMetricSpec
   */
  public function setRayMetricSpec(GoogleCloudAiplatformV1RayMetricSpec $rayMetricSpec)
  {
    $this->rayMetricSpec = $rayMetricSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1RayMetricSpec
   */
  public function getRayMetricSpec()
  {
    return $this->rayMetricSpec;
  }
  /**
   * @param string[]
   */
  public function setResourcePoolImages($resourcePoolImages)
  {
    $this->resourcePoolImages = $resourcePoolImages;
  }
  /**
   * @return string[]
   */
  public function getResourcePoolImages()
  {
    return $this->resourcePoolImages;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1RaySpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1RaySpec');
