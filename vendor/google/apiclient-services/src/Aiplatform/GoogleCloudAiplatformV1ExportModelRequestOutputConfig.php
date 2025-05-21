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

class GoogleCloudAiplatformV1ExportModelRequestOutputConfig extends \Google\Model
{
  protected $artifactDestinationType = GoogleCloudAiplatformV1GcsDestination::class;
  protected $artifactDestinationDataType = '';
  /**
   * @var string
   */
  public $exportFormatId;
  protected $imageDestinationType = GoogleCloudAiplatformV1ContainerRegistryDestination::class;
  protected $imageDestinationDataType = '';

  /**
   * @param GoogleCloudAiplatformV1GcsDestination
   */
  public function setArtifactDestination(GoogleCloudAiplatformV1GcsDestination $artifactDestination)
  {
    $this->artifactDestination = $artifactDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsDestination
   */
  public function getArtifactDestination()
  {
    return $this->artifactDestination;
  }
  /**
   * @param string
   */
  public function setExportFormatId($exportFormatId)
  {
    $this->exportFormatId = $exportFormatId;
  }
  /**
   * @return string
   */
  public function getExportFormatId()
  {
    return $this->exportFormatId;
  }
  /**
   * @param GoogleCloudAiplatformV1ContainerRegistryDestination
   */
  public function setImageDestination(GoogleCloudAiplatformV1ContainerRegistryDestination $imageDestination)
  {
    $this->imageDestination = $imageDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1ContainerRegistryDestination
   */
  public function getImageDestination()
  {
    return $this->imageDestination;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExportModelRequestOutputConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExportModelRequestOutputConfig');
