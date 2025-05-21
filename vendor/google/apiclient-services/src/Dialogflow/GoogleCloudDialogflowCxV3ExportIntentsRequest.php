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

class GoogleCloudDialogflowCxV3ExportIntentsRequest extends \Google\Collection
{
  protected $collection_key = 'intents';
  /**
   * @var string
   */
  public $dataFormat;
  /**
   * @var string[]
   */
  public $intents;
  /**
   * @var bool
   */
  public $intentsContentInline;
  /**
   * @var string
   */
  public $intentsUri;

  /**
   * @param string
   */
  public function setDataFormat($dataFormat)
  {
    $this->dataFormat = $dataFormat;
  }
  /**
   * @return string
   */
  public function getDataFormat()
  {
    return $this->dataFormat;
  }
  /**
   * @param string[]
   */
  public function setIntents($intents)
  {
    $this->intents = $intents;
  }
  /**
   * @return string[]
   */
  public function getIntents()
  {
    return $this->intents;
  }
  /**
   * @param bool
   */
  public function setIntentsContentInline($intentsContentInline)
  {
    $this->intentsContentInline = $intentsContentInline;
  }
  /**
   * @return bool
   */
  public function getIntentsContentInline()
  {
    return $this->intentsContentInline;
  }
  /**
   * @param string
   */
  public function setIntentsUri($intentsUri)
  {
    $this->intentsUri = $intentsUri;
  }
  /**
   * @return string
   */
  public function getIntentsUri()
  {
    return $this->intentsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3ExportIntentsRequest::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3ExportIntentsRequest');
