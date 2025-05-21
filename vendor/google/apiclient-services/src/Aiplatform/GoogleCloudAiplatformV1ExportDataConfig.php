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

class GoogleCloudAiplatformV1ExportDataConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $annotationSchemaUri;
  /**
   * @var string
   */
  public $annotationsFilter;
  /**
   * @var string
   */
  public $exportUse;
  protected $filterSplitType = GoogleCloudAiplatformV1ExportFilterSplit::class;
  protected $filterSplitDataType = '';
  protected $fractionSplitType = GoogleCloudAiplatformV1ExportFractionSplit::class;
  protected $fractionSplitDataType = '';
  protected $gcsDestinationType = GoogleCloudAiplatformV1GcsDestination::class;
  protected $gcsDestinationDataType = '';
  /**
   * @var string
   */
  public $savedQueryId;

  /**
   * @param string
   */
  public function setAnnotationSchemaUri($annotationSchemaUri)
  {
    $this->annotationSchemaUri = $annotationSchemaUri;
  }
  /**
   * @return string
   */
  public function getAnnotationSchemaUri()
  {
    return $this->annotationSchemaUri;
  }
  /**
   * @param string
   */
  public function setAnnotationsFilter($annotationsFilter)
  {
    $this->annotationsFilter = $annotationsFilter;
  }
  /**
   * @return string
   */
  public function getAnnotationsFilter()
  {
    return $this->annotationsFilter;
  }
  /**
   * @param string
   */
  public function setExportUse($exportUse)
  {
    $this->exportUse = $exportUse;
  }
  /**
   * @return string
   */
  public function getExportUse()
  {
    return $this->exportUse;
  }
  /**
   * @param GoogleCloudAiplatformV1ExportFilterSplit
   */
  public function setFilterSplit(GoogleCloudAiplatformV1ExportFilterSplit $filterSplit)
  {
    $this->filterSplit = $filterSplit;
  }
  /**
   * @return GoogleCloudAiplatformV1ExportFilterSplit
   */
  public function getFilterSplit()
  {
    return $this->filterSplit;
  }
  /**
   * @param GoogleCloudAiplatformV1ExportFractionSplit
   */
  public function setFractionSplit(GoogleCloudAiplatformV1ExportFractionSplit $fractionSplit)
  {
    $this->fractionSplit = $fractionSplit;
  }
  /**
   * @return GoogleCloudAiplatformV1ExportFractionSplit
   */
  public function getFractionSplit()
  {
    return $this->fractionSplit;
  }
  /**
   * @param GoogleCloudAiplatformV1GcsDestination
   */
  public function setGcsDestination(GoogleCloudAiplatformV1GcsDestination $gcsDestination)
  {
    $this->gcsDestination = $gcsDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsDestination
   */
  public function getGcsDestination()
  {
    return $this->gcsDestination;
  }
  /**
   * @param string
   */
  public function setSavedQueryId($savedQueryId)
  {
    $this->savedQueryId = $savedQueryId;
  }
  /**
   * @return string
   */
  public function getSavedQueryId()
  {
    return $this->savedQueryId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExportDataConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExportDataConfig');
