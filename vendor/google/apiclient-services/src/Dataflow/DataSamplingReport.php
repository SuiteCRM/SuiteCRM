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

namespace Google\Service\Dataflow;

class DataSamplingReport extends \Google\Model
{
  /**
   * @var string
   */
  public $bytesWrittenDelta;
  /**
   * @var string
   */
  public $elementsSampledBytes;
  /**
   * @var string
   */
  public $elementsSampledCount;
  /**
   * @var string
   */
  public $exceptionsSampledCount;
  /**
   * @var string
   */
  public $pcollectionsSampledCount;
  /**
   * @var string
   */
  public $persistenceErrorsCount;
  /**
   * @var string
   */
  public $translationErrorsCount;

  /**
   * @param string
   */
  public function setBytesWrittenDelta($bytesWrittenDelta)
  {
    $this->bytesWrittenDelta = $bytesWrittenDelta;
  }
  /**
   * @return string
   */
  public function getBytesWrittenDelta()
  {
    return $this->bytesWrittenDelta;
  }
  /**
   * @param string
   */
  public function setElementsSampledBytes($elementsSampledBytes)
  {
    $this->elementsSampledBytes = $elementsSampledBytes;
  }
  /**
   * @return string
   */
  public function getElementsSampledBytes()
  {
    return $this->elementsSampledBytes;
  }
  /**
   * @param string
   */
  public function setElementsSampledCount($elementsSampledCount)
  {
    $this->elementsSampledCount = $elementsSampledCount;
  }
  /**
   * @return string
   */
  public function getElementsSampledCount()
  {
    return $this->elementsSampledCount;
  }
  /**
   * @param string
   */
  public function setExceptionsSampledCount($exceptionsSampledCount)
  {
    $this->exceptionsSampledCount = $exceptionsSampledCount;
  }
  /**
   * @return string
   */
  public function getExceptionsSampledCount()
  {
    return $this->exceptionsSampledCount;
  }
  /**
   * @param string
   */
  public function setPcollectionsSampledCount($pcollectionsSampledCount)
  {
    $this->pcollectionsSampledCount = $pcollectionsSampledCount;
  }
  /**
   * @return string
   */
  public function getPcollectionsSampledCount()
  {
    return $this->pcollectionsSampledCount;
  }
  /**
   * @param string
   */
  public function setPersistenceErrorsCount($persistenceErrorsCount)
  {
    $this->persistenceErrorsCount = $persistenceErrorsCount;
  }
  /**
   * @return string
   */
  public function getPersistenceErrorsCount()
  {
    return $this->persistenceErrorsCount;
  }
  /**
   * @param string
   */
  public function setTranslationErrorsCount($translationErrorsCount)
  {
    $this->translationErrorsCount = $translationErrorsCount;
  }
  /**
   * @return string
   */
  public function getTranslationErrorsCount()
  {
    return $this->translationErrorsCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataSamplingReport::class, 'Google_Service_Dataflow_DataSamplingReport');
