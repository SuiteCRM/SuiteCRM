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

class GoogleCloudAiplatformV1AssignNotebookRuntimeRequest extends \Google\Model
{
  protected $notebookRuntimeType = GoogleCloudAiplatformV1NotebookRuntime::class;
  protected $notebookRuntimeDataType = '';
  /**
   * @var string
   */
  public $notebookRuntimeId;
  /**
   * @var string
   */
  public $notebookRuntimeTemplate;

  /**
   * @param GoogleCloudAiplatformV1NotebookRuntime
   */
  public function setNotebookRuntime(GoogleCloudAiplatformV1NotebookRuntime $notebookRuntime)
  {
    $this->notebookRuntime = $notebookRuntime;
  }
  /**
   * @return GoogleCloudAiplatformV1NotebookRuntime
   */
  public function getNotebookRuntime()
  {
    return $this->notebookRuntime;
  }
  /**
   * @param string
   */
  public function setNotebookRuntimeId($notebookRuntimeId)
  {
    $this->notebookRuntimeId = $notebookRuntimeId;
  }
  /**
   * @return string
   */
  public function getNotebookRuntimeId()
  {
    return $this->notebookRuntimeId;
  }
  /**
   * @param string
   */
  public function setNotebookRuntimeTemplate($notebookRuntimeTemplate)
  {
    $this->notebookRuntimeTemplate = $notebookRuntimeTemplate;
  }
  /**
   * @return string
   */
  public function getNotebookRuntimeTemplate()
  {
    return $this->notebookRuntimeTemplate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1AssignNotebookRuntimeRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1AssignNotebookRuntimeRequest');
