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

namespace Google\Service\Texttospeech;

class CustomPronunciationParams extends \Google\Model
{
  /**
   * @var string
   */
  public $phoneticEncoding;
  /**
   * @var string
   */
  public $phrase;
  /**
   * @var string
   */
  public $pronunciation;

  /**
   * @param string
   */
  public function setPhoneticEncoding($phoneticEncoding)
  {
    $this->phoneticEncoding = $phoneticEncoding;
  }
  /**
   * @return string
   */
  public function getPhoneticEncoding()
  {
    return $this->phoneticEncoding;
  }
  /**
   * @param string
   */
  public function setPhrase($phrase)
  {
    $this->phrase = $phrase;
  }
  /**
   * @return string
   */
  public function getPhrase()
  {
    return $this->phrase;
  }
  /**
   * @param string
   */
  public function setPronunciation($pronunciation)
  {
    $this->pronunciation = $pronunciation;
  }
  /**
   * @return string
   */
  public function getPronunciation()
  {
    return $this->pronunciation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CustomPronunciationParams::class, 'Google_Service_Texttospeech_CustomPronunciationParams');
