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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3ImportEntityTypesRequest extends \Google\Model
{
  protected $entityTypesContentType = GoogleCloudDialogflowCxV3InlineSource::class;
  protected $entityTypesContentDataType = '';
  /**
   * @var string
   */
  public $entityTypesUri;
  /**
   * @var string
   */
  public $mergeOption;
  /**
   * @var string
   */
  public $targetEntityType;

  /**
   * @param GoogleCloudDialogflowCxV3InlineSource
   */
  public function setEntityTypesContent(GoogleCloudDialogflowCxV3InlineSource $entityTypesContent)
  {
    $this->entityTypesContent = $entityTypesContent;
  }
  /**
   * @return GoogleCloudDialogflowCxV3InlineSource
   */
  public function getEntityTypesContent()
  {
    return $this->entityTypesContent;
  }
  /**
   * @param string
   */
  public function setEntityTypesUri($entityTypesUri)
  {
    $this->entityTypesUri = $entityTypesUri;
  }
  /**
   * @return string
   */
  public function getEntityTypesUri()
  {
    return $this->entityTypesUri;
  }
  /**
   * @param string
   */
  public function setMergeOption($mergeOption)
  {
    $this->mergeOption = $mergeOption;
  }
  /**
   * @return string
   */
  public function getMergeOption()
  {
    return $this->mergeOption;
  }
  /**
   * @param string
   */
  public function setTargetEntityType($targetEntityType)
  {
    $this->targetEntityType = $targetEntityType;
  }
  /**
   * @return string
   */
  public function getTargetEntityType()
  {
    return $this->targetEntityType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3ImportEntityTypesRequest::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3ImportEntityTypesRequest');
