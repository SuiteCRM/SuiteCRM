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

class GoogleCloudAiplatformV1CopyModelRequest extends \Google\Model
{
  protected $encryptionSpecType = GoogleCloudAiplatformV1EncryptionSpec::class;
  protected $encryptionSpecDataType = '';
  /**
   * @var string
   */
  public $modelId;
  /**
   * @var string
   */
  public $parentModel;
  /**
   * @var string
   */
  public $sourceModel;

  /**
   * @param GoogleCloudAiplatformV1EncryptionSpec
   */
  public function setEncryptionSpec(GoogleCloudAiplatformV1EncryptionSpec $encryptionSpec)
  {
    $this->encryptionSpec = $encryptionSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1EncryptionSpec
   */
  public function getEncryptionSpec()
  {
    return $this->encryptionSpec;
  }
  /**
   * @param string
   */
  public function setModelId($modelId)
  {
    $this->modelId = $modelId;
  }
  /**
   * @return string
   */
  public function getModelId()
  {
    return $this->modelId;
  }
  /**
   * @param string
   */
  public function setParentModel($parentModel)
  {
    $this->parentModel = $parentModel;
  }
  /**
   * @return string
   */
  public function getParentModel()
  {
    return $this->parentModel;
  }
  /**
   * @param string
   */
  public function setSourceModel($sourceModel)
  {
    $this->sourceModel = $sourceModel;
  }
  /**
   * @return string
   */
  public function getSourceModel()
  {
    return $this->sourceModel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1CopyModelRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1CopyModelRequest');
