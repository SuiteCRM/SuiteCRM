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

namespace Google\Service\Config;

class PreviewOperationMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $build;
  /**
   * @var string
   */
  public $logs;
  protected $previewArtifactsType = PreviewArtifacts::class;
  protected $previewArtifactsDataType = '';
  /**
   * @var string
   */
  public $step;

  /**
   * @param string
   */
  public function setBuild($build)
  {
    $this->build = $build;
  }
  /**
   * @return string
   */
  public function getBuild()
  {
    return $this->build;
  }
  /**
   * @param string
   */
  public function setLogs($logs)
  {
    $this->logs = $logs;
  }
  /**
   * @return string
   */
  public function getLogs()
  {
    return $this->logs;
  }
  /**
   * @param PreviewArtifacts
   */
  public function setPreviewArtifacts(PreviewArtifacts $previewArtifacts)
  {
    $this->previewArtifacts = $previewArtifacts;
  }
  /**
   * @return PreviewArtifacts
   */
  public function getPreviewArtifacts()
  {
    return $this->previewArtifacts;
  }
  /**
   * @param string
   */
  public function setStep($step)
  {
    $this->step = $step;
  }
  /**
   * @return string
   */
  public function getStep()
  {
    return $this->step;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PreviewOperationMetadata::class, 'Google_Service_Config_PreviewOperationMetadata');
