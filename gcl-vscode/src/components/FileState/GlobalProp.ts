import renderSection from "../Section";

export default function renderGlobalProps(
  globalProps: string[]
): string {
  return renderSection("Global Property", globalProps.join(" ∧</br>"))
}
