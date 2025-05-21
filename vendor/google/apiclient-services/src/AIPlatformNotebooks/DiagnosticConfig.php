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

namespace Google\Service\AIPlatformNotebooks;

class DiagnosticConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $enableCopyHomeFilesFlag;
  /**
   * @var bool
   */
  public $enablePacketCaptureFlag;
  /**
   * @var bool
   */
  public $enableRepairFlag;
  /**
   * @var string
   */
  public $gcsBucket;
  /**
   * @var string
   */
  public $relativePath;

  /**
   * @param bool
   */
  public function setEnableCopyHomeFilesFlag($enableCopyHomeFilesFlag)
  {
    $this->enableCopyHomeFilesFlag = $enableCopyHomeFilesFlag;
  }
  /**
   * @return bool
   */
  public function getEnableCopyHomeFilesFlag()
  {
    return $this->enableCopyHomeFilesFlag;
  }
  /**
   * @param bool
   */
  public function setEnablePacketCaptureFlag($enablePacketCaptureFlag)
  {
    $this->enablePacketCaptureFlag = $enablePacketCaptureFlag;
  }
  /**
   * @return bool
   */
  public function getEnablePacketCaptureFlag()
  {
    return $this->enablePacketCaptureFlag;
  }
  /**
   * @param bool
   */
  public function setEnableRepairFlag($enableRepairFlag)
  {
    $this->enableRepairFlag = $enableRepairFlag;
  }
  /**
   * @return bool
   */
  public function getEnableRepairFlag()
  {
    return $this->enableRepairFlag;
  }
  /**
   * @param string
   */
  public function setGcsBucket($gcsBucket)
  {
    $this->gcsBucket = $gcsBucket;
  }
  /**
   * @return string
   */
  public function getGcsBucket()
  {
    return $this->gcsBucket;
  }
  /**
   * @param string
   */
  public function setRelativePath($relativePath)
  {
    $this->relativePath = $relativePath;
  }
  /**
   * @return string
   */
  public function getRelativePath()
  {
    return $this->relativePath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DiagnosticConfig::class, 'Google_Service_AIPlatformNotebooks_DiagnosticConfig');
